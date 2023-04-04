library(devtools)
# Install concoR
#devtools::install_github("aslez/concoR")
library(concoR)
library(rio)
library(igraph)

### notice how even though our network started from an edgelist, we can convert it to a matrix easily
crime <- import("crime.csv")
crime_net <- graph.data.frame(crime, directed=T)

### turn edge list into a matrix object
mat <- as.matrix(get.adjacency(crime_net))

## Let's plot the network
### This is messy, difficult to see equivalent actors
windows()
plot(crime_net, edge.width=.1,  edge.arrow.size=.1, 
     vertex.color="white",
     vertex.label.color="black",
     vertex.shape="none")


  

## So what states do we think have similar structural equivalence in the network?
library(network)
library(sna)
## Prints out correlation between nodes int their connections
m0 <- cor(mat)
m0
View(m0)

## However, we want more than just simply correlation, we want to use a more 
## Sophisticated algorithm to test roles
## This will partition the data to identify states with similar roles in the network
blks <- concor_hca(list(mat)) 
blks

### We can assign those blocks to nodes, and color our network based on that.
windows()
V(crime_net)$blocks <- blks$block

plot.igraph(crime_net, vertex.color=V(crime_net)$blocks, edge.width=.1, edge.arrow.size=.1,      vertex.label.color="black")

### We can increase the number of blocks- p will
blks <- concor_hca(list(mat), p=2) 
blks

### We can assign those blocks to nodes, and color our network based on that.
windows()
V(crime_net)$blocks <- blks$block

plot.igraph(crime_net, vertex.color=V(crime_net)$blocks, edge.width=.1, edge.arrow.size=.1,      vertex.label.color="black")


### another approach- identifying common blocks of network


ec<-equiv.clust(mat, method="euclidean")

plot(ec, labels=ec$glabels, main="", xlab="", ylab="Height", sub="")

rect.hclust(ec$cluster,k=2,border="blue") 

bm<-blockmodel(mat,ec, k=2)
gplot(bm$block.model>gden(mat),diag=T,edge.lwd=bm$block.model*3,  main ="", displaylabels=TRUE)  


### maybe we don't think 2 is the correct division
plot(ec, labels=ec$glabels, main="", xlab="", ylab="Height", sub="")
rect.hclust(ec$cluster,k=3,border="blue") 


bm<-blockmodel(mat,ec, k=3)
gplot(bm$block.model>gden(mat),diag=T,edge.lwd=bm$block.model*3,  main ="", displaylabels=TRUE)  


### We can save our membership into a dataframe to use in other analysis. 

df <- as.data.frame(bm$block.membership)
df$block <- df$`bm$block.membership`
df$state <-bm$plabels
df$`bm$block.membership` <- NULL

