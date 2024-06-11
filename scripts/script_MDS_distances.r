#####
# How well MDS reconstructs the original data distances

# remember to set your working directory first
setwd("put the path to your directory here")
library(stylo)


resMDS = stylo(gui = FALSE, features = "wordlist.txt", frequencies = "table_with_frequencies.txt",mfw.min=1000,mfw.max=1000,write.png.file=FALSE,analysis.type="MDS",text.id.on.graphs="points")

#####
# use cmdscale function to compute MDS

gof = c()
for (i in 2:25){ # run the loop over all possible numbers of dimensions 
  mds.results = cmdscale(resMDS$distance.table, k=i, eig = TRUE)
  gof = c(gof,mds.results$GOF[1])
}
plot(gof) # Goodness of fit
plot(mds.results$eig) # analogous to PCA's "percentage of variance" thing

# the pairwise distances to be compared
mds.dist = dist(mds.results$points, method = "euclidean", diag = FALSE, upper = FALSE)
original.dist = dist.delta(resMDS$frequencies.0.culling[,1:1000])
plot(c(original.dist), c(mds.dist))
lines(matrix(c(0:2,0:2),c(3,3))) # the diagonal line of equal distances

{ # another possibility is using isometric MDS (for non-euclidean spaces)
  library(MASS)
  iso.results = isoMDS(resMDS$distance.table, cmdscale(resMDS$distance.table))
  iso.dist = dist(iso.results$points, method = "euclidean", diag = FALSE, upper = FALSE)
  
  plot(c(original.dist), c(iso.dist), col = "red")
  points(c(original.dist), c(mds.dist))
  lines(matrix(c(0:2,0:2),c(3,3)))
}
  

#####
# We are going outside of 'stylo', 
# so we make sure that the output plot is the same

{ # copy-pasting some variables for plotting from stylo
groups = gsub("_.*","",rownames(resMDS$distance.table))
variables = stylo.default.settings()
add.to.margins = variables$add.to.margins
label.offset = variables$label.offset
plot.line.thickness = variables$plot.line.thickness
colors.on.graphs = variables$colors.on.graphs
colors.of.pca.graph = assign.plot.colors(labels = groups, col = colors.on.graphs, opacity = 1)
}

xy.coord = mds.results$points[,1:2]
# xy.coord = iso.results$points[,1:2]

plot.area = define.plot.area(xy.coord[,1], xy.coord[,2],
                             xymargins = add.to.margins,
                             v.offset = label.offset)
plot(xy.coord, type = "p",
     ylab = "", xlab = "",
     xlim = plot.area[[1]], ylim = plot.area[[2]],
     # main = graph.main.title,
     # sub = graph.subtitle,
     col = colors.of.pca.graph,
     lwd = plot.line.thickness)

