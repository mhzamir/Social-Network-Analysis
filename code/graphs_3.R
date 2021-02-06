# Let's try to understand the characteristics of Communities 1 and 5
#I chose this communities because they have more nodes which might bring more insight
# when trying to understand Spammers and Legitimates
install.packages("igraph")
library("igraph")

NODES  <- read.csv((paste(HOME,"users/coded_ids.csv", sep="")), header=T, as.is=T)

combined_edges_w_2 <- read.csv((paste(HOME,"graphs/c_combined_edges_w_2_s_0.9.csv", sep="")), header=T, as.is=T)
net2 <- graph_from_data_frame(d=combined_edges_w_2, vertices=NODES, directed=T)
net2_sub <- delete.vertices(simplify(net2), degree(net2)==0)

#Global clustering coefficient
local_clustering = transitivity(net2_sub, type = "local")
summary(local_clustering) #NA's 46

#Removing CC = NaNs and CCs == 0
for(i in seq(length(local_clustering), 1)){
  if (is.na(local_clustering[i]) || local_clustering[i]==0){
    net2_sub = delete_vertices(net2_sub, i)
  }
}


#Greedy Communities
#https://users.dimi.uniud.it/~massimo.franceschet/R/communities.html
c1 = cluster_fast_greedy(as.undirected(net2_sub))
# modularity matrix
B = modularity_matrix(net2_sub, membership(c1))
round(B[1,],2)

#Topological properties of the network and its communities: n is the number of nodes, d
#the density,Global Clustering Coefficient, the average distance, kmax the maximal degree



#Code Ref:
#https://ourednik.info/maps/2018/09/21/create-a-subgraph-from-the-neighborhood-of-specific-vertices-in-igraph/
topologicalData <- function(list_nodes, net2_sub) {
  selnodes <- V(net2_sub)[name %in% list_nodes]
  
  # turn the returned list objects into a graph
  selegoG <- induced_subgraph(net2_sub,unlist(selnodes))
  
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
  print("Centralize")
  print(centralize(degree(selegoG),normalize=FALSE)/((vcount(selegoG)-1)*(vcount(selegoG)-2)))
  
}

nodes_of_interest_1  <- c("1",  "18",  "25",  "34",  "48",  "60",  "91",  "122", "149", "168", "170", "183", "185", "196", "198", "229", "232", "233", "236", "237", "238", "239", "244", "251", "260")
nodes_of_interest_2  <- c("8","26",  "169", "192", "197", "255")
nodes_of_interest_3  <- c("86",  "92",  "108", "109", "110")
nodes_of_interest_4  <- c("63", "65", "66", "68", "70")
nodes_of_interest_5  <- c("3",   "9",   "10",  "17",  "20",  "33",  "35",  "84",  "89",  "98",  "102", "136", "137", "138", "144", "147",
                          "160", "166", "171", "179", "180", "187", "188", "201", "205", "207", "214", "218", "220", "223", "227", "240",
                          "241", "250", "257", "262", "264", "265", "267", "309", "315", "463")
nodes_of_interest_6  <- c("142", "163", "164", "182", "200", "245", "252", "254")
nodes_of_interest_7  <- c("4",   "56",  "194", "226")
nodes_of_interest_8  <- c("74", "75", "76")
nodes_of_interest_9  <- c("124", "125", "126")
nodes_of_interest_10 <- c("64", "67", "71")
nodes_of_interest_11 <- c("59",  "61",  "231")
nodes_of_interest_12 <-c("80",  "81",  "119")


topologicalData(nodes_of_interest_5,net2_sub)
