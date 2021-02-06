#Exploring App User Interaction Graph

install.packages("igraph")
library("igraph")


NODES  <- read.csv((paste(HOME,"users/coded_ids.csv", sep="")), header=T, as.is=T)

### NODES Should be the app_id
NODES_APP  <- read.csv((paste(HOME,"graphs/app_based_similarity/apps_ids.csv", sep="")), header=T, as.is=T)

coded_weighted_edges <- read.csv((paste(HOME,"graphs/app_based_similarity/app_user_interaction_graph/coded_weighted_edges.csv", sep="")), header=T, as.is=T)


#Code Ref:
#https://ourednik.info/maps/2018/09/21/create-a-subgraph-from-the-neighborhood-of-specific-vertices-in-igraph/
topologicalData <- function(selegoG) {
 
  print("Number of Nodes")
  print(length(V(selegoG))) #Number of Nodes
  print("Density")
  print(edge_density(selegoG, loops=TRUE)) #Density
  print("Global Clustering Coefficient")
  print(transitivity(selegoG))#Global Clustering Coefficient
  print("Max  distance")
  print(diameter(selegoG, directed=T, weights=NA))# max  distance
  print("Average distance")
  print(mean_distance(selegoG))# average distance (degree of separation)
  print("Max Degree")
  print(max(degree(selegoG))) #Max Degree
  print("Max In Degree")
  print(max(degree(selegoG, mode = c("in"))))
  print("Max Out Degree")
  print(max(degree(selegoG, mode = c("out")))) 
}

#https://stackoverflow.com/questions/28874028/creating-a-bipartite-graph-in-r-with-igraph-with-specific-edge-list

nodesSet1 = NODES_APP$app_id
nodesSet2 = NODES$coded_id

edgeList <- data.frame(S1=coded_weighted_edges$app_id,
                       S2=coded_weighted_edges$user_id)


# first we give prefixes to the nodes to discern the two partition
g <- graph.empty()
g <- add.vertices(g,nv=length(nodesSet1),attr=list(name=paste0('A',nodesSet1),
                                                   type=rep(TRUE,length(nodesSet1))))
g <- add.vertices(g,nv=length(nodesSet2),attr=list(name=paste0('B',nodesSet2),
                                                   type=rep(FALSE,length(nodesSet2))))

# we need to turn edgeList into a vector (and using names instead of indexes)
edgeListVec <- as.vector(t(as.matrix(data.frame(S1=paste0('A',edgeList$S1),
                                                S2=paste0('B',edgeList$S2)))))
g <- add.edges(g,edgeListVec)

# check if is recognized as bipartite
is.bipartite(g)


# let's plot it !
V(g)$label <- V(g)$name
V(g)$label.cex = 0.5
plot.igraph(g, layout=layout.bipartite, vertex.color=c("orange","green")[V(g)$type+1],  edge.arrow.size=0.0000000000000001, vertex.size=3, vertex.label=NA)

topologicalData(g)
degree(g)

 
#plot_7_corrected 
g_sub <- delete.vertices(simplify(g), degree(g)==0)
plot.igraph(g_sub, vertex.size=2, layout=layout_nicely, edge.arrow.size=0.00000001,vertex.label=NA, vertex.color=c("orange","green")[V(g_sub)$type+1])

mean(degree(g)) #4.118126
g_sub_possible_abusives <- delete.vertices(simplify(g_sub), degree(g_sub)<5)
#plot_8_corrected 
plot.igraph(g_sub_possible_abusives, vertex.size=5, layout=layout_nicely, edge.arrow.size=0.00000001,vertex.label.size=3, vertex.color=c("orange","green")[V(g_sub_possible_abusives)$type+1])


plot.igraph(g_sub_possible_abusives, layout=layout.bipartite, vertex.color=c("orange","green")[V(g_sub_possible_abusives)$type+1],  edge.arrow.size=0.0000000000000001, vertex.size=2, vertex.label=NA)

#LIST all the degrees and present the top 10 abusive users and applications
degree(g_sub_possible_abusives)
apps = c("77","19","176","98","109","146","102")
users = c("714","669","664","461","473","394","349","341","285","283")
