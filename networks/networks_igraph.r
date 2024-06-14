#set your working directory (if you're using RStudio)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Packages
# install.packages("igraph")
# library(help="igraph")
library(igraph)
#sessionInfo

###
### From crashR.R demo
###

### Graphs
## Create a small graph, A->B, A->C, B->C, C->E, D
## A=1, B=2, C=3, D=4, E=5
g <- graph( c(1,2, 1,3, 2,3, 3,5), n=5 )
g

### Summary, number of vertices, edges
summary(g)
vcount(g)
ecount(g)

### Create an undirected graph as well
## A--B, A--C, B--C, C--E, D
g2 <- graph( c(1,2, 1,3, 2,3, 3,5), n=5, dir=FALSE )
g2

### Is the graph directed?
is_directed(g)
is_directed(g2)

### Convert from directed to undirected
as.undirected(g)
### And back
as.directed(as.undirected(g))

### Multiple edges and loops
g <- graph( c(4,4,1,2,1,2, 1,3, 2,3, 4,5), n=5 )
plot(g,edge.arrow.size=0.1)
### Remove multiple edges
g <- simplify(g)
plot(g)
is_simple(g)

### Naming vertices
V(g)$name <- letters[1:5]
g
plot(g)

### Create undirected example graph
g2 <- graph_from_literal(Alice-Bob:Cecil:Daniel, 
                         Cecil:Daniel-Eugene:Gordon )
V(g2)$name
g2
plot(g2)

### Remove Alice
g3 <- delete_vertices(g2, 1)
g3 <- delete_vertices(g2, match("Alice", V(g2)$name))

### Add three new vertices
g4 <- add_vertices(g3, 3)
print(g4, v=T)
igraph_options(print.vertex.attributes=TRUE, 
               plot.layout=layout_with_kk)
plot(g4)                      

### Add three new vertices, with names this time
g4 <- add_vertices(g3, 3, attr=list(name=c("Helen", "Ike", "Jane")))
### Add some edges as well
g4 <- add_edges(g4, match(c("Helen", "Jane", "Ike", "Jane"), V(g4)$name ))
plot(g4)


### Edge sequences, first create a directed example graph
g2 <- graph_from_literal(Alice -+ Bob:Cecil:Daniel, 
                         Cecil:Daniel +-+ Eugene:Gordon )
plot(g2, layout=layout_with_kk, vertex.label=V(g2)$name)
### Sequence of all edges
E(g2)
### Edge from a vertex to another
E(g2, P=c(1,2))
### Get the id of the edge
as.vector(E(g2, P=c(1,3)))
### Outgoing edges
E(g2)[ from(3) ]
### Incoming edges
E(g2)[ to(3) ]
### Edges along a path
E(g2, path=c(1,4,5))
### All adjacent edges of a vertex
E(g2)[ adj(3) ]
### Or multiple vertices
E(g2)[ adj(c(3,1)) ]
g3 <- subgraph.edges(g2, E(g2)[
  adj(match("Cecil",V(g2)$name)) ], delete.vertices = TRUE)
plot(g3, layout=layout_with_kk,edge.arrow.size =0.1)
plot(g2,edge.arrow.size =0.1)

################
# TASK 1:
# - check if the network is directed/weighted/
# - check what attributes do the edges and nodes have
# - plot it with various layouts: layout_with_kk,layout_with_fr, layout_in_circle, layout_with..... (you choose :)
# - plot ego network for Anna
# Source of data: Micha³ Paradowski and Andrzej Jarynowski, network of interactions between a group of L2 learners of Polish at the Warsaw University
# Attributes: weight - general interactions, intensityPL - interactions in Polish language
################

load("polonicum.Rdata")

################
# Some answers
################

net_SLA
is_directed(net_SLA)
is_weighted(net_SLA)
plot(net_SLA, layout=layout_in_circle,edge.arrow.size =0.1)

E(net_SLA)$weight
E(net_SLA)$generalInt <- E(net_SLA)$weight
E(net_SLA)$intensityPL

g3 <- subgraph.edges(net_SLA, 
                     E(net_SLA)[adj(1) ], delete.vertices = TRUE)
g3 <- subgraph.edges(net_SLA, 
                     E(net_SLA)[adj(match("Anna",V(net_SLA)$name)) ], delete.vertices = TRUE)
plot(g3, layout=layout_with_kk,edge.arrow.size =0.1)

################
# End of task
################


###
### Adapted from centrality.R demo and smallworld.R
###

g <- graph_from_literal(Andre----Beverly:Diane:Fernando:Carol,
                        Beverly--Andre:Diane:Garth:Ed, # change to directed
                        Carol----Andre:Diane:Fernando,
                        Diane----Andre:Carol:Fernando:Garth:Beverly,
                        Ed-------Beverly:Garth,
                        Fernando-Carol:Andre:Diane:Garth:Heather,
                        Garth----Ed:Beverly:Diane:Fernando:Heather,
                        Heather--Fernando:Garth:Ike,
                        Ike------Heather:Jane,
                        Jane-----Ike )
### Define its plotting properties
V(g)$label <- V(g)$name # Labels (shown in the plot) the same as names
V(g)$color <- "white" # color for all nodes
V(g)$frame.color <- "black"
V(g)[name=="Heather"]$color <- "orange" # color for individual nodes
V(g)$size <- 20
V(g)$label.cex <- 1.5
E(g)$color <- "black"
E(g)$width <- 3
### Hand-drawn coordinates 
coords <- c(5,5,119,256,119,256,120,340,478,
            622,116,330,231,116,5,330,451,231,231,231) #pairs of numbers in sequence
coords <- matrix(coords, nc=2) # and now in tabular form
g$layout <- coords #<- layout_in_circle # etc.
plot(g, asp=FALSE,vertex.label.font=2)

### Add clustering centrality to labels
# V(g)$label <- paste(sep="\n", V(g)$name, degree(g))
V(g)$label <- paste(sep="\n", V(g)$name, round(transitivity(g, type="local"),2))
plot(g, asp=FALSE)
### Add an edge and recalculate transitivity
g1 <- add_edges(g, V(g)[name %in% c("Fernando","Ike")], color="red", width=3)
V(g1)$label <- paste(sep="\n", V(g1)$name, round(transitivity(g1, type="local"),2))
plot(g1, asp=FALSE)

# Watch what happens with transitivity for a directed graph (check definitions of local clustering coefficient)
V(g1)$label <- paste(sep="\n", V(g1)$name, round(transitivity(as.directed(g1), type="local"),2))
plot(g1, asp=FALSE)

# Watch what happens for disconnected graphs (check definition of closeness)
closeness(g1)
g1 <- delete.edges(g1,c(17))
plot(g1, asp=FALSE)
closeness(g1)
components(g1)


####################################
### The Zachary Karate club network
karate <- make_graph("Zachary")
summary(karate)
### Create a layout that is used from now on
karate$layout <- layout_with_kk(karate)
V(karate)$color <- "cyan"
plot(karate,asp=FALSE)

# compute degree assortativity
assortativity_degree(karate)


### Correlation between centrality measures
cent <- list(`Degree`=degree(karate),
             `Closeness`=closeness(karate),
             `Betweenness`=betweenness(karate),
             `Eigenvector`=eigen_centrality(karate)$vector,
             `PageRank`=page_rank(karate)$vector,
             `LocalClustering`=transitivity(karate, type="local"),
             `Authority`=authority_score(karate)$vector,
             `Hubness`=hub_score(karate)$vector)

# Replot edge thickness and node size based on centrality measures
eb <- edge.betweenness(karate)
E(karate)$width <- 10*eb/max(eb)
V(karate)$size <- 20*cent$Betweenness/max(cent$Betweenness)
plot(karate)

### Pairs plot
pairs(cent, lower.panel=function(x,y) {
  usr <- par("usr")
  text(mean(usr[1:2]), mean(usr[3:4]), round(cor(x,y), 3), cex=1, col="blue")
} )


################
# TASK 2:
# - compute various centralities: degree, closeness, betwenness, hub/authority
# - plot vertex sizes according to their centrality
# - plot edge width using weights
# - filter out edges with intensityPL<1
# - discuss social roles of the nodes
################

load("polonicum.Rdata")

################
# Some answers
################

# compute in- and out-degrees
degree(net_SLA, mode = c("all", "out", "in", "total"))

E(net_SLA)$weight <- E(net_SLA)$intensityPL

cent <- list(`Degree`=strength(net_SLA, mode = "out"))
V(net_SLA)$size <- 20*cent$Degree/max(cent$Degree)

plot(net_SLA, layout=layout_with_fr,edge.arrow.size =0.1)

g3 <- subgraph.edges(net_SLA, 
                     E(net_SLA)[intensityPL>1], delete.vertices = TRUE)
plot(g3, layout=layout_with_kk,edge.arrow.size =0.1)


################
# End of task
################


###
### From cohesive.R demo
###


### The Zachary Karate club network
karate <- make_graph("Zachary")
karate$layout <- layout_with_kk(karate)
plot(karate)

### Run cohesive blocking on it
cbKarate <- cohesive_blocks(karate)
cbKarate

### Plot the results and all the groups
plot(cbKarate,karate, col = "cyan")

### This is a bit messy, plot them step-by-step
### See the hierarchy tree first
hierarchy(cbKarate)
plot_hierarchy(cbKarate)
cbKarate$cohesion
cbKarate$parent

## Plot the first level, blocks 1 & 2
plot(cbKarate, karate, mark.groups=blocks(cbKarate)[1:2+1])

### The first group has more subgroups, plot them
sub1 <- blocks(cbKarate)[parent(cbKarate)==1]
plot(cbKarate, karate, mark.groups=sub1)
sub2 <- blocks(cbKarate)[parent(cbKarate)==2]
plot(cbKarate, karate, mark.groups=sub2)
### Plot more cohesive subgroup
sub3 <- blocks(cbKarate)[cohesion(cbKarate)>2]
plot(cbKarate, karate, mark.groups=sub3)

# cliques(karate, min=4, max=4)
cl <- largest_cliques(karate)
plot(karate,mark.groups=cl)
### Zoom in to this part
axis(1)
axis(2)
abline(h=c(-.3, .7)) # might need: library(graphics)
abline(v=c(-0.2,0.6))
plot(karate,mark.groups=cl, xlim=c(-0.1,0.5), ylim=c(-.1, .5),frame=TRUE)


###
### From community.R demo
###

### Plot the adjacency matrix, use the Matrix package if available
if (require(Matrix)) {
  myimage <- function(...) image(Matrix(...))
} else {
  myimage <- image
}

### Create a hierarchical network
pref.mat <- matrix(0, 16, 16)
pref.mat[1:4,1:4] <- pref.mat[5:8,5:8] <-
  pref.mat[9:12,9:12] <- pref.mat[13:16,13:16] <- 7.5/127
pref.mat[ pref.mat==0 ] <- 1/(3*128)
diag(pref.mat) <- diag(pref.mat) + 10/31
### Create the network with the given vertex preferences
G <- sample_pref(16*16, types=16, pref.matrix=pref.mat)
myimage(pref.mat)
lay <- layout_with_fr(G)
plot(G,layout=lay)


### Run spinglass community detection with two gamma parameters
sc1 <- cluster_spinglass(G, spins=4, gamma=1.0)
sc2.2 <- cluster_spinglass(G, spins=16, gamma=2.2)
### Plot it with community (=component) colors
plot(G, vertex.color=membership(sc1), layout=lay,asp=FALSE)
plot(G, vertex.color=membership(sc2.2), layout=lay,asp=FALSE)


modularity(G, 1:vcount(G))
modularity(G, c(rep(1,64),rep(2,64),rep(3,64),rep(4,64)))
modularity(G, membership(sc1))
modularity(G, membership(sc2.2))

# Plot adjacency matrix
A <- as_adj(G)
myimage(A)
# WHY is it so random. We know there are four communities!


### Rearrange adjacency matrix according to (big) communities
ord1 <- order(membership(sc1))
myimage(A[ord1,ord1])
### Rearrange adjacency matrix according to (small) communities
ord2.2 <- order(membership(sc2.2))
myimage(A[ord2.2,ord2.2])


#############################
### Back to Zachary's karate club data
karate <- make_graph("Zachary")
karate$layout <- layout_with_kk(karate)

### Greedy algorithm
fc <- cluster_fast_greedy(karate)
memb <- membership(fc)
plot(karate, vertex.color=memb, asp=FALSE)
### Easier plotting
plot(fc, karate, asp=FALSE)

### Comparision of algorithms
communities <- list()
fc <- cluster_fast_greedy(karate) # cluster_fast_greedy
communities$`Fast greedy` <- fc
lec <- cluster_leading_eigen(karate) # cluster_leading_eigen
communities$`Leading eigenvector` <- lec
sc <- cluster_spinglass(karate, spins=10) # cluster_spinglass
communities$`Spinglass` <- sc
louvain <- cluster_louvain(karate) # cluster_label_prop
communities$`Louvain` <- louvain
infomap <- cluster_infomap(karate) # cluster_label_prop
communities$`Infomap` <- infomap
ebc <- cluster_edge_betweenness(karate) # cluster_edge_betweenness
communities$`Edge betweenness` <- ebc
# wt <- cluster_walktrap(karate) # cluster_walktrap
# communities$`Walktrap` <- wt
# labprop <- cluster_label_prop(karate) # cluster_label_prop
# communities$`Label propagation` <- labprop

### Plot everything
layout(rbind(1:3, 4:6))
coords <- layout_with_kk(karate)
lapply(seq_along(communities), function(x) {
  m <- modularity(communities[[x]])
  par(mar=c(1,1,3,1))
  plot(communities[[x]], karate, layout=coords,
       main=paste(names(communities)[x], "\n",
                  "Modularity:", round(m, 3)))
})
dev.off()

################
# TASK 3:
# - can you find social motiffs (e.g., cliques, triangles, lone wolves, communities) and interpret them?
# - find community detection algorithm obtaining highest modularity value
# - check how edge weights affect the above results (see,e.g., help(cluster_louvain))
################

load("polonicum.Rdata")

################
# Some answers
################

### Comparision of algorithms
communities <- list()
sc <- cluster_spinglass(net_SLA, spins=10) # cluster_spinglass
communities$`Spinglass` <- sc
wt <- cluster_walktrap(net_SLA) # cluster_walktrap
communities$`Walktrap` <- wt
labprop <- cluster_label_prop(net_SLA) # cluster_label_prop
communities$`Label propagation` <- labprop

### Plot everything
layout(rbind(1:3))
coords <- layout_with_fr(net_SLA)
lapply(seq_along(communities), function(x) {
  m <- modularity(communities[[x]])
  par(mar=c(1,1,3,1))
  plot(communities[[x]], net_SLA, layout=coords,edge.arrow.size=0.1,
       main=paste(names(communities)[x], "\n",
                  "Modularity:", round(m, 3)))
})
dev.off()

communities <- list()
communities$`Spinglass` <- sc <- cluster_spinglass(as.undirected(net_SLA), spins=10) # cluster_spinglass
communities$`Greedy` <- fc <- cluster_fast_greedy(as.undirected(net_SLA)) # cluster_fast_greedy not for directed
communities$`Eigenvector` <- lec <- cluster_leading_eigen(as.undirected(net_SLA)) # cluster_leading_eigen
communities$`Louvain` <- louvain <- cluster_louvain(as.undirected(net_SLA)) # cluster_label_prop
communities$`Infomap` <- infomap <- cluster_infomap(as.undirected(net_SLA)) # cluster_label_prop
communities$`EdgeBetweenness` <- ebc <- cluster_edge_betweenness(as.undirected(net_SLA)) # cluster_edge_betweenness

### Plot everything
layout(rbind(1:3,4:6))
coords <- layout_with_fr(net_SLA)
lapply(seq_along(communities), function(x) {
  m <- modularity(communities[[x]])
  par(mar=c(1,1,3,1))
  plot(communities[[x]], net_SLA, layout=coords,edge.arrow.size=0.1,
       main=paste(names(communities)[x], "\n",
                  "Modularity:", round(m, 3)))
})
dev.off()

################
# End of task
################
