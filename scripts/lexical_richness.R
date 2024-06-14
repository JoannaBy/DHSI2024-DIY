setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(stylo)

setwd("C:/OneDrive - Uniwersytet Jagielloński/Naukowe/DigitalHum/DHSI2024-DIY/classify-tests/")

# Load corpus and tokenise it using default splitting rule
corp=load.corpus.and.parse(files = "Dickens_Bleak.txt",
                           corpus.dir = "corpus", features = "w",
                           ngram.size = 1, preserve.case = FALSE,
                           encoding = "UTF-8")

booklen <- length(corp$Dickens_Bleak)

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
for(i in seq(1,round(booklen/incr))){
  # Make a frequency list of word unigrams
  freqs <- make.frequency.list(corp$Dickens_Bleak[(1+(i-1)*incr):(i*incr)]
                               ,value = TRUE)
  # Compute entropy
  entropy <- sum(-freqs*log(freqs), na.rm = TRUE)
  # Append it to the vector of results
  results <- c(results, entropy)
}

plot(seq(1,round(booklen/incr))*incr,results, type = 'l',
     xlab = 'words',ylab='entropy')


################
# Look at how entropy of word frequency distribution increases along the novel
# See also: https://en.wikipedia.org/wiki/Heaps%27_law

incr = 2000
results <- c()
for(i in seq(1,round(booklen)/incr)){
  # Make a frequency list of word unigrams
  freqs <- make.frequency.list(corp$Dickens_Bleak[1:(i*incr)], value = TRUE)
  # Compute entropy
  entropy <- sum(-freqs*log(freqs), na.rm = TRUE)
  # Append it to the vector of results
  results <- c(results, entropy)
}


plot(seq(1,round(booklen/incr))*incr,results, type = 'l',
     xlab = 'words',ylab='entropy')

#####
# Try using other packages that implement lexical diversity measures
# install.packages("quanteda")
# install.packages("quanteda.textstats")
library("quanteda")
library("quanteda.textstats")

library(stylo)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load a book with stylo as a list of words
filelist = list.files(pattern = "*.txt") # get the list of files in that directory
corp = load.corpus.and.parse(files = filelist, corpus.dir = "",corpus.lang = "English")
book = corp$Dickens_Bleak

iterator = 3600
res.ent = c()
for (t in seq(iterator,length(book),iterator)) {
  
  seg = paste(corp$Dickens_Bleak[1:t], collapse=" ") # concatenate tokens
  x = tokens(seg) # tokenise again with quanteda package
  x = dfm(x) # compute term-frequency matrix
  res.ent = c(res.ent, textstat_entropy(x)$entropy[1]) # compute entropy
  
}

plot(res.ent)

#####
# Other lexical diversity measures  
# iterator = 1000
# res = c()
# for (t in seq(iterator,length(book),iterator)) {
#   
#   seg = paste(corp$Dickens_Bleak[1:t], collapse=" ")
#   x = tokens(seg)
#   res = c(res, textstat_lexdiv(x, measure = "D")$D[1])
#   
# }
# 
# plot(res)
