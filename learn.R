learn.matrix <- function(path, alph, prob=FALSE) {
  m <- matrix(0, nrow=length(alph), ncol=length(alph))
  rownames(m) <- alph
  colnames(m) <- alph
  for (line in readLines(path, encoding="UTF-8")) {
    if (line=="" | line == " " | line == "\r") next
    line <- trim.line(line, alph)
    m["\n", substr(line, 1, 1)] <- m["\n", substr(line, 1, 1)] + 1
    for (i in 1:(nchar(line)-1)) {
      current <- substr(line, i, i) 
      then <- substr(line, i+1, i+1)
      if (!then %in% alph) print(then)
      m[current, then] <- m[current, then] + 1
    }
    m[substr(line, nchar(line), nchar(line)), "\n"] <- m[substr(line, nchar(line), nchar(line)), "\n"] + 1
  }
  if (prob) return(m/rowSums(m))
  else return(m)
}

jump.matrix <- function(text, alph) {
  m <- matrix(0, nrow=length(alph), ncol=length(alph))
  rownames(m) <- alph
  colnames(m) <- alph
  for (i in 1:(nchar(text)-1)) {
    current <- substr(text, i, i)
    then <- substr(text, i+1, i+1)
    m[current, then] <- m[current, then] + 1
  }
  return(m)
}

# Necessary for removing junk characters left over after UTF-8 conversion
trim.line <- function(line, alph) {
  while (nchar(line)>0 & !(substr(line, 1, 1) %in% alph)) {
    line <- substr(line, 2, nchar(line))
  }
  while (nchar(line)>0 & !(substr(line, nchar(line), nchar(line)) %in% alph)) {
    line <- substr(line, 1, nchar(line)-1)
  }
  return(line)
}

read.text <- function(path, alph) {
  str <- ''
  for (line in readLines(path)) {
    line <- trim.line(line, alph)
    if (line=="" | line == " " | line == "\r") next
    str <- paste(str, line, sep="\n")
  }
  return(substr(str, 2, nchar(str))) # Remove the first line break
}

poly.matrices <- function(text, p, alph) {
  mat <- matrix(0, length(alph), length(alph))
  rownames(mat) <- alph
  colnames(mat) <- alph
  l <- rep(list(mat), p)
  alphabet <- 1
  for (i in 1:(nchar(text)-1)) {
    current <- substr(text, i, i)
    then <- substr(text, i+1, i+1)
    l[[alphabet%s%p]][current, then] <- l[[alphabet%s%p]][current, then] + 1
    alphabet <- alphabet + 1
  }
  return(l)
}