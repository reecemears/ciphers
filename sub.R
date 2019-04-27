solve.sub <- function(ciphertext, alph, max.iterations) {
  t0.i <- get.t0.i(ciphertext)
  sigma <- 1:length(alph) # Start with the identity, i.e. the ciphertext is already decoded
  M.sigma <- jump.matrix(ciphertext, alph)
  logb.sigma <- new.log.b(sigma, t0.i)
  decoding <- ciphertext
  sigma.l <- 0
  best10 <- PriorityQueue$new()
  best10$push(decoding, -1*sigma.l)
  record <- 0
  for (it in 1:max.iterations) {
    if (it%%10000==0) print(it)
    t <- transposition(sigma)
    phi <- t[[1]]; i <- t[[2]]; j <- t[[3]]
    M.phi <- new.M(M.sigma, i, j)
    logb.phi <- new.log.b(phi, t0.i)
    acc <- acc.prob(sigma.l, M.sigma, M.phi, logb.sigma, logb.phi, i, j)
    alpha <- acc[[1]]; phi.l <- acc[[2]]
    if (rbinom(1, 1, alpha)) { # If the move is accepted
      sigma <- phi
      M.sigma <- M.phi
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

write.sub <- function(path, ciphertext, alph, max.iterations, original='') {
  out <- solve.sub(ciphertext, alph, max.iterations)
  best10 <- out[[1]]; acc <- out[[2]]
  fileConn <- file(path)
  lines <- c("Encoded message:", ciphertext, "\n------------------------\n")
  lines <- c(lines, paste("Executed", max.iterations, "iterations, acceptance probability =", acc), 
             "\n------------------------\n")
  for (i in best10$size():1) {
    decoded <- use.key.sub(ciphertext, reverse.sub.key(best10$data[[i]]))
    metadata <- paste0("#", 11-i, "\n")
    if (nchar(original)>0) metadata <- paste0(metadata, "Accuracy: ", score(decoded, original), "\n")
    lines <- c(lines, metadata, decoded, "\n------------------------\n")
  }
  writeLines(lines, fileConn)
  close(fileConn)
}

new.M <- function(M, i, j) {
  ij <- c(i, j)
  order <- transposition(1:nrow(M), ij)[[1]]
  new <- M[order, order]
  return(new)
}

new.log.b <- function(phi, t0.i) {
  return(logbeta[phi[t0.i]])
}

get.t0.i <- function(ciphertext) {
  return(which(alph==substr(ciphertext, 1, 1)))
}

acc.prob <- function(sigma.l, M.sigma, M.phi, logb.sigma, logb.phi, i, j) {
  diff <- M.phi-M.sigma
  loga <- (logb.phi-logb.sigma) + sum(diff*logP)
  return(list(min(c(1, exp(loga))), loga+sigma.l))
}

transposition <- function(sigma, ij=NA) {
  if (sum(is.na(ij))>0) ij <- sample(sigma, 2)
  i <- min(ij)
  j <- max(ij)
  if (i==1) start <- c()
  else start <- 1:(i-1)
  if (j==length(sigma)) end <- c()
  else end <- (j+1):length(sigma)
  if (abs(i-j)<=1) middle <- c()
  else middle <- (i+1):(j-1)
  return(list(sigma[c(start, j, middle, i, end)], i, j))
}