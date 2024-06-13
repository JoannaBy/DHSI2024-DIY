setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(stylo)

res = stylo()

# How to compute...
p = res$table.with.all.freqs[1,]
# entropy 
sum(-p*log(p), na.rm = TRUE)
# and Simpsons index
sum(p^2, na.rm = TRUE)
