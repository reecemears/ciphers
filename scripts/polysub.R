solve.polysub <- function(ciphertext, p, alph, max.iterations) {
  n <- length(alph)
  t0.i <- get.t0.i(ciphertext)
  sigma <- matrix(rep(1:n,p), p, n, byrow=TRUE)
  M.sigma <- poly.matrices(ciphertext, p, alph)
  sum.sigma <- add.matrices(M.sigma)
  logb.sigma <- new.log.b.poly(sigma, t0.i)
  decoding <- ciphertext
  sigma.l <- 0
  best10 <- PriorityQueue$new()
  best10$push(decoding, -1*sigma.l)
  record <- 0
  for (it in 1:max.iterations) {
    if (it%%10000==0) print(it)
    t <- transposition.poly(sigma)
    phi <- t[[1]]; r <- t[[2]]; i <- t[[3]]; j <- t[[4]]
    M.phi <- new.M.poly(M.sigma, r, i, j, p)
    sum.phi <- new.sum(sum.sigma, M.sigma, M.phi, r, i, j, p)
    logb.phi <- new.log.b(phi, t0.i)
    acc <- acc.prob.poly(sigma.l, sum.sigma, sum.phi, logb.sigma, logb.phi)
    alpha <- acc[[1]]; phi.l <- acc[[2]]
    if (rbinom(1, 1, alpha)) { # If the move is accepted
      sigma <- phi
      M.sigma <- M.phi
      sum.sigma <- sum.phi
      logb.sigma <- logb.phi
      sigma.l <- phi.l
      record <- record + 1
      if (best10$size()<10 | -1*phi.l<max(best10$priorities)) {
        exists <- FALSE
        for (i in 1:10) {
          if (isTRUE(all.equal(-1*phi.l, best10$priorities[i]))) {
            exists <- TRUE
            break
          }
        }
        if (!exists) {
          if (best10$size()>=10) best10$pop()
          best10$push(phi, -1*phi.l)
        }
      }
    }
  }
  return(list(best10, record/max.iterations))
}

write.polysub <- function(path, ciphertext, p, alph, max.iterations, original='') {
  out <- solve.polysub(ciphertext, p, alph, max.iterations)
  best10 <- out[[1]]; acc <- out[[2]]
  fileConn <- file(path)
  lines <- c("Encoded message:", ciphertext, "\n------------------------\n")
  lines <- c(lines, paste("Executed", max.iterations, "iterations, acceptance probability =", acc), 
             "\n------------------------\n")
  for (i in best10$size():1) {
    decoded <- use.key.polysub(ciphertext, reverse.sub.key(best10$data[[i]]))
    metadata <- paste0("#", 11-i, "\n")
    if (nchar(original)>0) metadata <- paste0(metadata, "Accuracy: ", score(decoded, original), "\n")
    lines <- c(lines, metadata, decoded, "\n------------------------\n")
  }
  writeLines(lines, fileConn)
  close(fileConn)
}

new.log.b.poly <- function(phi, t0.i) {
  return(logbeta[phi[1,t0.i]])
}

new.M.poly <- function(M.sigma, r, i, j, p) {
  # rows
  temp <- M.sigma[[r]][i,]
  M.sigma[[r]][i,] <- M.sigma[[r]][j,]
  M.sigma[[r]][j,] <- temp
  # columns
  temp <- M.sigma[[(r-1)%s%p]][,i]
  M.sigma[[(r-1)%s%p]][,i] <- M.sigma[[(r-1)%s%p]][,j]
  M.sigma[[(r-1)%s%p]][,j] <- temp
  return(M.sigma)
}

new.sum <- function(sum.sigma, M.sigma, M.phi, r, i, j, p) {
  ms <- list(sum.sigma, -1*M.sigma[[r]], -1*M.sigma[[(r-1)%s%p]], M.phi[[r]], M.phi[[(r-1)%s%p]])
  return(add.matrices(ms))
}

transposition.poly <- function(sigma) {
  p <- nrow(sigma); n <- ncol(sigma)
  r <- sample(1:p,1)
  ij <- sample(1:n, 2)
  i <- min(ij); j <- max(ij)
  temp <- sigma[r,i]
  sigma[r,i] <- sigma[r,j]
  sigma[r,j] <- temp
  return(list(sigma, r, i, j))
}

acc.prob.poly <- function(sigma.l, sum.sigma, sum.phi, logb.sigma, logb.phi) {
  diff <- sum.phi-sum.sigma
  loga <- (logb.phi-logb.sigma) + sum(diff*logP)
  return(list(min(c(1, exp(loga))), loga+sigma.l))
}

add.matrices <- function(l) {
  s <- matrix(0, nrow=nrow(l[[1]]), ncol=ncol(l[[1]]))
  for (m in l) {
    s <- s + m
  }
  return(s)
}
