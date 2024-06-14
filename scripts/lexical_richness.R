setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(stylo)

setwd("C:/OneDrive - Uniwersytet Jagielloński/Naukowe/DigitalHum/DHSI2024-DIY/classify-tests/")

# Load corpus and tokenise it using default splitting rule
corp=load.corpus.and.parse(files = "Dickens_Bleak.txt",
                           corpus.dir = "corpus", features = "w",
                           ngram.size = 1, preserve.case = FALSE,
                           encoding = "UTF-8")

freqs <- make.frequency.list(corp$Dickens_Bleak ,value = TRUE)

# How to compute...
p = freqs
# entropy 
sum(-p*log(p), na.rm = TRUE)
# and Simpsons index
sum(p^2, na.rm = TRUE)

################
# Look at how entropy of word frequency distribution changes along the novel


incr = 2000
results <- c()
for(i in seq(1,round(length(corp$Dickens_Bleak)/incr))){
  # Make a frequency list of word unigrams
  freqs <- make.frequency.list(corp$Dickens_Bleak[(1+(i-1)*incr):(i*incr)]
                               ,value = TRUE)
  # Compute entropy
  entropy <- sum(-freqs*log(freqs), na.rm = TRUE)
  # Append it to the vector of results
  results <- c(results, entropy)
}

plot(results, type = 'l')

################
# Look at how entropy of word frequency distribution increases along the novel
# See also: https://en.wikipedia.org/wiki/Heaps%27_law

incr = 2000
results <- c()
for(i in seq(1,round(length(corp$Dickens_Bleak)/incr))){
  # Make a frequency list of word unigrams
  freqs <- make.frequency.list(corp$Dickens_Bleak[1:(i*incr)]
                               ,value = TRUE)
  # Compute entropy
  entropy <- sum(-freqs*log(freqs), na.rm = TRUE)
  # Append it to the vector of results
  results <- c(results, entropy)
}

plot(results, type = 'l')
