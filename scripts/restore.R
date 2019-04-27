restore <- function() {
  logP <- log(learn.matrix('../resources/warandpeace.txt', alph, prob=TRUE))
  logP[logP==-Inf] <- -12
  M <- learn.matrix('../resources/warandpeace.txt', alph)
  logbeta <- log(rowSums(M)/sum(M))
}
