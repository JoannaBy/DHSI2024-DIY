library(stylo)

res=stylo(gui = FALSE, features = "wordlist.txt", frequencies = "table_with_frequencies.txt",mfw.min=1000,mfw.max=1000,write.png.file=FALSE,analysis.type="PCV")
pca.results=prcomp(res$table.with.all.freqs[,1:3], scale=FALSE, center = TRUE)
pca.results
res1=stylo(gui = FALSE, features = "wordlist.txt", frequencies = "table_with_frequencies.txt",mfw.min=1000,mfw.max=1000,write.png.file=FALSE,analysis.type="PCR",text.id.on.graphs="points")

# Percent of variance in principal components
round(res$pca.sdev^2/sum(res$pca.sdev^2)*100,2)
res$pca.var.exp

# Show explained variance in a graph
stylo(gui = FALSE, features = "wordlist.txt", frequencies = "table_with_frequencies.txt",mfw.min=1000,mfw.max=1000,write.png.file=FALSE,analysis.type="PCV",pca.visual.flavour="technical")

# Show which words contributed to the principal components most
res2=stylo(gui = FALSE, features = "wordlist.txt", frequencies = "table_with_frequencies.txt",mfw.min=1000,mfw.max=1000,write.png.file=FALSE,analysis.type="PCV",pca.visual.flavour="loadings")

# To get more components you need to write own code
plot(pca.results$rotation[1:3,1:2])
plot(pca.results$rotation[1:3,2:3])