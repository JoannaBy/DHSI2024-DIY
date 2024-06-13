### Packages
#install.packages("igraph")
# library(help="igraph")
library(igraph)

#set your working directory (if you're using RStudio)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#########################
# Noun & adjective adjacency network (from: http://www-personal.umich.edu/~mejn/netdata/)
#########################
# The file adjnoun.gml contains the network of common adjective and noun
# adjacencies for the novel "David Copperfield" by Charles Dickens, as
# described by M. Newman.  Nodes represent the most commonly occurring
# adjectives and nouns in the book.  Node values are 0 for adjectives and 1
# for nouns.  Edges connect any pair of words that occur in adjacent position
# in the text of the book.  Please cite M. E. J. Newman, Finding community
# structure in networks using the eigenvectors of matrices, Preprint
# physics/0605087 (2006).
#########################

g <- read_graph("adjnoun.net",format="pajek")
is.weighted(g)
g <- as.undirected(g)
comm <- cluster_louvain(g)
plot(g,vertex.color=membership(comm),vertex.size=10,layout=layout_with_fr)

# Additionally import labels:"adj"/"noun"
lbls <- read.csv("adjnoun_label.csv", sep = "\t")
# lbls[,2]==V(g)$name # OK, the order of the words is the same
# And check is nouns and adjectives like each other's company
ass <- assortativity_nominal(g,types = lbls[,2])

# Would random graphs with the same degree sequence
# give you the same assortativity?
a <- c()
for(i in 1:100){
  gR <- sample_degseq(degree(g))
  a[i] <- assortativity_nominal(gR,types = lbls[,2])
}
hist(a)
abline(v=ass)

#########################
# Make your own word adjacency network
#########################
# Source: https://github.com/computationalstylistics/A_Small_Collection_of_British_Fiction
# Classic English 19th c. novels
#########################

library(stylo)

# Load corpus and tokenise it using default splitting rule
corp=load.corpus.and.parse(files = "ABronte_Agnes.txt", corpus.dir = "corpus", features = "w",ngram.size = 2, preserve.case = FALSE,encoding = "UTF-8")
# Make a frequency list of word 2-grams
freqs <- make.frequency.list(corp$ABronte_Agnes,value = TRUE)
# split the 2-grams to get network nodes and make an edge list
el <- strsplit(rownames(freqs)," ")
el <- unlist(el)
# make an edge list
el <- matrix(el,ncol = 2,byrow = TRUE)

g <- graph_from_edgelist(el,directed = TRUE)
E(g)$weight=as.numeric(freqs)
E(g)$width=as.numeric(freqs)*20
g1 <- subgraph.edges(g, E(g)[weight>0.07], delete.vertices = TRUE)
plot(g1, layout=layout_with_fr,edge.arrow.size=0.2)


#########################
# Network of book similarities
#########################
# Source: https://github.com/computationalstylistics/A_Small_Collection_of_British_Fiction
# Classic English 19th c. novels
# Method for constructing network: Eder, M. (2015). Visualization in stylometry: cluster analysis using networks. Digital Scholarship in the Humanities, 30
#########################

library(stylo)

# Load corpus and tokenise it using default splitting rule
corp=load.corpus.and.parse(files = "all", corpus.dir = "corpus", features = "w",ngram.size = 1, preserve.case = FALSE,encoding = "UTF-8")

# Pass the parsed corpus to stylo (GUI should pop-up)
res <- stylo(gui = TRUE, frequencies = NULL, parsed.corpus = corp, features = NULL)

# Make the stylometric network
res1 <- stylo(network=TRUE, network.type="undirected", parsed.corpus = corp, frequencies = NULL, features = NULL)
# Put it into igraph
el <- as.matrix(res1$list.of.edges[,1:2])
ws <- as.numeric(res1$list.of.edges[,3])
g <- graph_from_edgelist(el,directed = FALSE)
E(g)$width <- ws
plot(g)

# Alternatively try if the following works (a web browser should open)
res1 <- stylo.network(parsed.corpus = corp, frequencies = "table_with_frequencies.txt")



#########################
# Network of Marvel comic heroes (courtesy of Padraig Mac Carron)
#########################
# Gleiser P. M., J. Stat. Mech. (2007) P09020.
# Alberich R., Miro-Julia J. and Rosell´o F., condmat/0202174.
#########################

# Load table with edges
comics <- read.csv("comic.csv", sep = "\t")
comics[1,]
el <- as.matrix(comics[,1:2])
g <- graph_from_edgelist(el,directed = FALSE)
# Separate comic books from heroes
books <- V(g)[name %in% el[,2]]$name
heroes <- V(g)[name %in% el[,1]]$name
# To get hero network we need to do a trick (thanks to the graph being bipartite)
adj <- as_adjacency_matrix(g) # get adjacency matrix
adj <- adj %*% adj # make a one-step walk (from heroes to books, from books to heroes)
# sum(adj[heroes,books]) # now all heroes are disconnected from books
adj <- adj[heroes,heroes]
gH <- graph_from_adjacency_matrix(adj,weighted = TRUE,mode = "undirected")
is.simple(gH)
gH <- simplify(gH) # get rid of multiple edges and loops
is.weighted(gH)

heroes.appearances <- diag(as.matrix(adj))
gHH <- induced_subgraph(gH,order(heroes.appearances,decreasing = TRUE)[1:50])
### Define its plotting properties
V(gHH)$size <- heroes.appearances[order(heroes.appearances,decreasing = TRUE)[1:50]]/50
V(gHH)$label.cex <- sqrt(heroes.appearances[order(heroes.appearances,decreasing = TRUE)[1:50]]/2000)
plot(gHH,frame= TRUE)


###
### From centrality.R demo
###


### A real network, US supreme court citations
##  You will need internet connection for this to work
vertices <- read.csv("http://jhfowler.ucsd.edu/data/judicial.csv")
edges <- read.table("http://jhfowler.ucsd.edu/data/allcites.txt")
jg <- graph.data.frame(edges, vertices=vertices, dir=TRUE)

### Basic data
summary(jg)
### Is it a simple graph?
is_simple(jg)
### Is it connected?
is_connected(jg)
### How many components?
count_components(jg)
### How big are these?
table(components(jg)$csize)
### In-degree distribution
plot(degree_distribution(jg, mode="in"), log="xy")
### Out-degree distribution
plot(degree_distribution(jg, mode="out"), log="xy")
### Largest in- and out-degree, total degree
c(max(degree(jg, mode="in")),
  max(degree(jg, mode="out")),
  max(degree(jg, mode="all")))
### Density ??
edge_density(g)# density(jg)
### Transitivity
transitivity(jg)
### Transitivity of a random graph of the same size
g <- sample_gnm(vcount(jg), ecount(jg))
transitivity(g)
### Transitivity of a random graph with the same degree distribution
g <- sample_degseq(degree(jg, mode="out"), degree(jg, mode="in"),
                   method="simple")
transitivity(g)
### Authority and Hub scores
AS <- authority_score(jg)#$vector
HS <- hub_score(jg)$vector
### Time evolution of authority scores
AS <- authority_score(jg)$vector
center <- which.max(AS)
startyear <- V(jg)[center]$year
### Function to go back in time
auth.year <- function(y) {
  print(y)
  keep <- which(V(jg)$year <= y)
  g2 <- subgraph(jg, keep)
  as <- abs(authority_score(g2, scale=FALSE)$vector)
  w <- match(V(jg)[center]$usid, V(g2)$usid)
  as[w]
}
### Go back in time for the top authority, do a plot
AS2 <- sapply(startyear:2005, auth.year)
plot(startyear:2005, AS2, type="b", xlab="year", ylab="authority score")
### Check another case
center <- "22US1"
startyear <- V(jg)[center]$year
### Calculate past authority scores & plot them - not working?
AS3 <- sapply(startyear:2005, auth.year)
plot(startyear:2005, AS3, type="b", xlab="year", ylab="authority score")