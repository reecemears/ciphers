encode.sub <- function(plaintext, alph) {
  n <- length(alph)
  key <- sample(n, n)
  encoded <- ''
  for (i in 1:nchar(plaintext)) {
    encoded <- paste0(encoded, alph[key[alph==substr(plaintext, i, i)]])
  }
  return(encoded)
}

encode.trans <- function(plaintext, alph, t) {
  encoded <- ''
  N <- nchar(plaintext)
  key <- sample(1:t, size=t)
  for (i in seq(1, N, t)) {
    chunk <- substr(plaintext, i, min(i+t-1, N))
    spl <- strsplit(chunk, '')[[1]]
    spl <- c(spl, rep('', t-length(spl)))
    ordered <- paste(spl[key], collapse='')
    encoded <- paste0(encoded, ordered)
  }
  return(encoded)
}

encode.polysub <- function(plaintext, alph, p) {
  n <- length(alph)
  key <- t(apply(matrix(0,p,1), 1, function(x) sample(n,n)))
  encoded <- ''
  index <- 1
  for (i in 1:nchar(plaintext)) {
    encoded <- paste0(encoded, alph[key[(1:p)[index%s%p], alph==substr(plaintext, i, i)]])
    index <- index + 1
  }
  return(encoded)
}

encode.vigenere <- function(plaintext, alph, keyword='', p=0) {
  n <- length(alph)
  if (nchar(keyword)==0 && p > 0) key <- sample(1:n, p, replace=TRUE) 
  else if (nchar(keyword)>0) {
    key <- c()
    for (char in strsplit(keyword, '')[[1]]) {
      key <- c(key, which(alph==char))
    }
    p <- length(key)
  }
  else stop('Provide either a keyword or key length')
  key <- key - 1
  encoded = ''
  index <- 1
  for (i in 1:nchar(plaintext)) {
    encoded <- paste0(encoded, alph[(which(alph==substr(plaintext, i, i))+key[index%s%p])%s%n])
    index <- index + 1
  }
  return(encoded)
}

# Shifted mod function
`%s%` <- function(x, y) {
  if (x%%y==0) return(y)
  else return(x%%y)
}

reverse.sub.key <- function(key) {
  if (is.null(dim(key))) {
    rev <- numeric(length(key))
    for (i in 1:length(key)) rev[key[i]] <- i
  } else {
    rev <- matrix(0, nrow=nrow(key), ncol=ncol(key))
    for (i in 1:nrow(key)) {
      for (j in 1:ncol(key)) {
        rev[i,key[i,j]] <- j
      }
    }
  }
  return(rev)
}
