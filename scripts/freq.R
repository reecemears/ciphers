plot.freq <- function(text, title='') {
  barplot(table(strsplit(text, ''))/nchar(text), main=title, 
          xlab='Character', ylab='Relative frequency')
}

char.freq <- function(text) {
  return(table(strsplit(text, ''))/nchar(text))
}

d <- char.freq(extract)
for (i in 1:length(d)) {
  freqs$ext[freqs$char==names(d)[i]] <- unname(d[i])
}

# barplot(t(as.matrix(freqs[81:1,-char])), names.arg=freqs$char[81:1], beside=TRUE, horiz=TRUE)
