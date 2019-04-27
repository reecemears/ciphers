log.likelihood <- function(text, beta=TRUE) {
  N <- nchar(text)
  if (beta) l.l <- logbeta[which(alph%in%c(substr(text, 1, 1)))]
  else l.l <- 0
  for (i in 1:(N-1)) {
    here <- substr(text, i, i)
    there <- substr(text, i+1, i+1)
    l.l <- l.l + logP[here, there]
  }
  return(unname(l.l))
}

solve.naive <- function(ciphertext, alph, max.iterations, write=FALSE) {
  # Start with identity permutation
  n <- length(alph)
  sigma <- 1:n
  sigma.l <- log.likelihood(ciphertext, logP, alph)
  decoding <- ciphertext
  if (write) lines <- c(ciphertext, "\n------------------------\n", paste("Initial likelihood:", sigma.l))
  record <- 0
  #print(paste0("Initial likelihood: ", sigma.l))
  for (it in 1:max.iterations) {
    # Now propose phi, a transposition of sigma
    if (it%%5000==0) print(it)
    phi <- transposition(sigma)
    ij <- which(phi!=sigma)
    i <- min(ij); j <- max(ij)
    decoding <- swapLetters(alph[i], alph[j], decoding)
    phi.l <- log.likelihood(decoding, logP, alph)
    #print(paste0("New likelihood: ", phi.l))
    acc <- exp(phi.l-sigma.l)
    alpha <- min(c(1, acc)) # acceptance probability
    flip <- rbinom(1, 1, alpha)
    if (write) lines <- c(lines, paste("Changed mapping for", alph[i], alph[j], ":", phi.l, "Alpha:", alpha, "Accepted?", as.logical(flip)), decoding)
    if (flip) {
      # If the move is accepted
      #print("Accepted")
      sigma <- phi
      sigma.l <- phi.l
      record <- record + 1
    } else decoding <- swapLetters(alph[i], alph[j], decoding)
  }
  if (write) return(c(lines, "\n------------------------\n", 
                      encodeSub(ciphertext, alph, key=matrix(sigma, nrow=1))[[1]], 
                      "\n------------------------\n", 
                      paste("Prob. of acceptance:", record/max.iterations)))
  return(phi)
}

write.naive <- function(path, ciphertext, alph, max.iterations) {
  lines <- solve.naive(ciphertext, alph, max.iterations, write=TRUE)
  fileConn<-file(path)
  writeLines(lines, fileConn)
  close(fileConn)
}
