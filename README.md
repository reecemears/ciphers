# Simulation-based approaches for cracking classical ciphers
This project uses Markov chain Monte Carlo to crack simple substitution, simple transposition and polyalphabetic substitution ciphers. This work complements my third year dissertation towards the degree of BSc Data Science with the University of Warwick.

## Initialisation:
If your R environment does not contain the matrix `logP`, or the vectors `logbeta` and `alph`, then run the following command.

```r
restore()
```

If using a different training text, replace the file path with that of the new .txt file. The package `liqueueR` is required for decoding texts, and can be installed and loaded using:

```r
install.packages('liqueueR')
library(liqueueR)
```

## Encoding texts:
There are four types of cipher implemented for encryption: simple substitution, simple transposition, polysub and Vigenere. Each of these ciphers has a function `encode.<cipher-name>()`, that takes the plaintext, alphabet, and any cipher specific parameters. See the functions themselves for more information on these parameters. These functions then return a character variable of the encoded text. For inputting text as a character variable in the R environment, use the function `read.text()` rather than default R functions. This will remove extra whitespacing and will ensure the character variable is the correct format for the later functions.

## Decoding texts:
There are three solvers for encoded texts: simple substitution, simple transposition and polysub. Note that the Vigenere texts are also solved with the polysub solver. If the decryption is to be used in later R script, is required in memory, ..., then use `solve.<cipher-name>()`, providing the ciphertext, alphabet, maximum iterations and cipher specific parameters. This will return a list of two elements: a PriorityQueue object of the top ten decryptions, and the overall acceptance rate of the Metropolis-Hastings algorithm. If the decryption is to be used once, outside of the R environment, ..., then use `write.<cipher-name>()`, providing a path for the resulting .txt file to save to, as well as the parameters from the `solve` version. This will list the original message, statistics from the runtime of the algorithm, and then the best ten decryptions found.

## Testing and scoring:
Average performances from the algorithms can be found in the file `tests.R`, where there are separate test cases for each of the functions. The `score()` function is a direct comparison between two pieces of text, and returns a percentage indicating how similar they are at the character level.
