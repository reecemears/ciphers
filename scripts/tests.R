test.sub <- function(plaintext, alph, iteration.range, repeats) {
  maxs <- c()
  avgs <- c()
  for (it in iteration.range) {
    ciphertext <- encode.sub(plaintext, alph)
    accuracies <- c()
    for (i in 1:repeats) {
      pr <- solve.sub(ciphertext, alph, it)[[1]]
      for (key in pr$data) {
        decoded <- use.key.sub(ciphertext, reverse.sub.key(key))
        accuracies <- c(accuracies, score(decoded, plaintext))
      }
    }
    maxs <- c(maxs, max(accuracies))
    avgs <- c(avgs, mean(accuracies))
  }
  plot(log10(iteration.range), avgs, ylim=c(0,1), ylab='Accuracy', xlab='Log number of iterations', type='l',
       main=paste('Accuracy of decoding substitution ciphers for varying iterations with', repeats, 'repeats'))
  lines(log10(iteration.range), maxs, lty=2)
  abline(h=1, lty=3)
  abline(h=0, lty=3)
  legend('bottomright', legend=c('Average accuracy', 'Maximum accuracy'), lty=c(1,2))
}

sub.length <- function(plaintext, percents, repeats) {
  N <- nchar(plaintext)
  lengths <- floor(N*(percents/100))
  maxs <- c()
  avgs <- c()
  for (length in lengths) {
    accuracies <- c()
    text <- substr(plaintext, 1, length)
    ciphertext <- encode.sub(text, alph)
    for (i in 1:repeats) {
      pr <- solve.sub(ciphertext, alph, 1e+06)[[1]]
      for (key in pr$data) {
        decoded <- use.key.sub(ciphertext, reverse.sub.key(key))
        accuracies <- c(accuracies, score(decoded, substr(plaintext, 1, length)))
      }
    }
    maxs <- c(maxs, max(accuracies))
    avgs <- c(avgs, mean(accuracies))
  }
  plot(lengths, avgs, ylim=c(0,1), ylab='Accuracy', xlab='Length of ciphertext', type='l',
       main=paste('Accuracy of decoding substitution ciphers for varying ciphertext lengths with', repeats, 'repeats'))
  lines(lengths, maxs, lty=2)
  abline(h=1, lty=3)
  abline(h=0, lty=3)
  legend('bottomright', legend=c('Average accuracy', 'Maximum accuracy'), lty=c(1,2))
}

test.trans <- function(plaintext, alph, t.range, iteration.range, repeats) {
  for (fast in c(TRUE, FALSE)) {
    count <- 1
    plot(0,type='n', ylab="Perfect decryptions (%)", xlab="Cipher key length", bty='L',
         main=paste("Success of transposition cipher\ndecryption with", repeats, 
                    "repeat(s) with fast=", fast), ylim=c(0,100), xlim=c(min(t.range), max(t.range)))
    legend('bottomleft', legend=formatC(iteration.range, format = "e", digits = 0), 
           lty=1:length(iteration.range), col='black', title="Iterations")
    for (max.iterations in iteration.range) {
      record <- c()
      for (t in t.range) {
        t.record <- 0
        for (i in 1:repeats) {
          e <- encode.trans(plaintext, alph, t)
          pr <- solve.trans(e, t, alph, max.iterations)[[1]]
          decoded <- use.key.trans(e, pr$data[[pr$size()]])
          if (score(decoded, plaintext)==1) t.record <- t.record + 1
        }
        record <- c(record, t.record)
        print(paste("Done", t, "for", max.iterations, "iterations"))
      }
      record <- 100*record/repeats
      lines(t.range, record, lty=count)
      count <- count + 1
    }
  }
}

bucket.comparisons <- function(N, tmax) {
  # fast = TRUE
  curve(min(6,x)*round(2000/x), from=2, to=tmax, xlab='t', ylab='Number of transitions used')
  abline(h=2000, lty=2)
  legend('right', legend=c('Bucketing', 'Chen-Rosenthal'), col='black', lty=1:2)
  # fast = FALSE
  lengths <- c()
  for (i in 7:tmax) {
    lengths <- c(lengths, min(N, (2000*i/6) - ((2000*i/6)%%i)))
  }
  plot(7:tmax, lengths, ylab='Amount of ciphertext used', xlab='t', type='l')
  lines(2:6, rep(2000, 5))
  abline(h=2000, lty=2)
  legend('right', legend=c('Bucketing', 'Chen-Rosenthal'), col='black', lty=1:2)
}

test.polysub <- function(plaintext, p.range, iteration.range, repeats) {
  plot(0, ylim=c(0,1), xlim=c(min(p.range), max(p.range)), ylab='Maximum accuracy', xlab='p', type='n', xaxt='n',
       main=paste('Maximum accuracy of decoding polysub ciphers for varying iterations with', repeats, 'repeats'))
  legend('bottomleft', legend=formatC(iteration.range, format = "e", digits = 0), 
         lty=1:length(iteration.range), col='black', title="Iterations")
  axis(side=1, at=p.range, cex.axis=1)
  count <- 1
  for (it in iteration.range) {
    maxs <- c()
    for (p in p.range) {
      accuracies <- c()
      ciphertext <- encode.polysub(plaintext, alph, p)
      for (i in 1:repeats) {
        pr <- solve.polysub(ciphertext, p, alph, it)[[1]]
        for (key in pr$data) {
          decoded <- use.key.polysub(ciphertext, reverse.sub.key(key))
          accuracies <- c(accuracies, score(decoded, plaintext))
        }
      }
      maxs <- c(maxs, max(accuracies))
    }
    lines(p.range, maxs, lty=count)
    count <- count + 1
  } 
}

poly.speed <- function(alph, p) {
  n <- length(alph)
  start <- Sys.time()
  key <- t(apply(matrix(0,p,1), 1, function(x) sample(n,n)))
  print(Sys.time()-start)
  start <- Sys.time()
  key <- matrix(0, p, n)
  for (i in 1:nrow(key)) {
    key[i,] <- sample(n, n)
  }
  print(Sys.time()-start)
}

test.suite <- function() {
  test.sub(extract, alph, c(1e+03, 1e+04, 1e+05, 1e+06, 1e+07), 3)
  sub.length(substr(magicshop,1,5000), c(1,2,5,seq(10,100,10)), 3)
  test.trans(magicshop, alph, 5:40, c(1e+03, 5e+03, 1e+04, 5e+04), 3)
  test.polysub(extract, 2:5, c(1e+06, 1e+07, 1e+08), 3)
}
