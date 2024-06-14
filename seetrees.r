# get words explaining the clustering like here: https://github.com/perechen/seetrees

install.packages("devtools")
devtools::install_github("perechen/seetrees")

library(stylo)
library(seetrees)


stylo_res <- stylo()
view_tree(stylo_res, k=2,right_margin=12) ## redraws a dendrogram based on distance matrix, cuts it to k groups, shows associated features 
