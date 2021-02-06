# Exploratory graphs
# Source: 
# [1] https://bookdown.org/rdpeng/exdata/exploratory-graphs.html

install.packages("igraph")
library("igraph")



NODES  <- read.csv((paste(HOME,"users/coded_ids.csv", sep="")), header=T, as.is=T)
combined_edges_w_1 <- read.csv((paste(HOME,"graphs/c_combined_edges_w_1_s_0.9.csv", sep="")), header=T, as.is=T)
combined_edges_w_2 <- read.csv((paste(HOME,"graphs/c_combined_edges_w_2_s_0.9.csv", sep="")), header=T, as.is=T)

net <- graph_from_data_frame(d=combined_edges_w_1, vertices=NODES, directed=T)
#class(net)
#net

#File Name: plot_1
#In this grah we can see that there are many nodes that does not share the same content. T
#Therefore, just a small subset of nodes are connected by sharing the same content
plot(net,vertex.size=2, edge.arrow.size=0.0000000000000001,vertex.label=NA)
diam = diameter(net, directed=T, weights=NA) # 7
centrality_cent = centralize(degree(net),normalize=FALSE)/((vcount(net)-1)*(vcount(net)-2)) # 0.066124
density = edge_density(net, loops=TRUE) #0.002269293

E(net) # The edges of the "net" object 1335/1335
V(net) # The vertices of the "net" object 767/767 vertices
E(net)$Sim # Edge attribute "type" 
V(net)$user_id # Vertex attribute "media"

#File Name: plot_2
#In order to visualize better, lets check the nodes with degree > 0
#Remove vertices with 0 degree
#Code source: https://stackoverflow.com/questions/50350196/applying-threshold-on-specific-vertices-weight
net_sub <- delete.vertices(simplify(net), degree(net)==0)
plot.igraph(net_sub, vertex.size=2, edge.arrow.size=0.0000000000000001,vertex.label=NA)
E(net_sub) #1335/1335   
V(net_sub) #295/295 vertices   

#Weight threshold 
net2 <- graph_from_data_frame(d=combined_edges_w_2, vertices=NODES, directed=T)
plot(net2,vertex.size=2, edge.arrow.size=0.0000000000000001,vertex.label=NA)
diam = diameter(net2, directed=T, weights=NA) #6
centrality_cent = centralize(degree(net2),normalize=FALSE)/((vcount(net2)-1)*(vcount(net2)-2)) #0.02823086
density = edge_density(net2, loops=TRUE) #0.0009332148

E(net2) # 549/549 
V(net2) #  767/767 vertices

# Weight threshold + degree > 0
#File Name: plot_3
net2_sub <- delete.vertices(simplify(net2), degree(net2)==0)
plot.igraph(net2_sub, vertex.size=2, edge.arrow.size=0.0000000000000001,vertex.label=NA)
E(net2_sub) #549/549 edges
V(net2_sub) #157/157 

diam = diameter(net2_sub, directed=T, weights=NA) #6
centrality_cent = centralize(degree(net2_sub),normalize=FALSE)/((vcount(net2_sub)-1)*(vcount(net2_sub)-2)) #0.1039289 
density = edge_density(net2_sub, loops=TRUE) #0.02227271 


#Global clustering coefficient
transitivity(net2_sub) #0.801076
local_clustering = transitivity(net2_sub, type = "local")
summary(local_clustering) #NA's 46

#Removing CC = NaNs and CCs == 0
for(i in seq(length(local_clustering), 1)){
  if (is.na(local_clustering[i]) || local_clustering[i]==0){
    net2_sub = delete_vertices(net2_sub, i)
  }
}



#File Name: plot_4
### 47 V removed 46 with NaN Local Clustering and 1 with Zero
plot.igraph(net2_sub, vertex.size=2, edge.arrow.size=0.0000000000000001,vertex.label=NA)
E(net2_sub) # 520/520 edges
V(net2_sub) # 110/110 vertices

diam = diameter(net2_sub, directed=T, weights=NA) # 5
centrality_cent = centralize(degree(net2_sub),normalize=FALSE)/((vcount(net2_sub)-1)*(vcount(net2_sub)-2)) #0.1265715 
density = edge_density(net2_sub, loops=TRUE) #0.04297521 


#Greedy Communities
#https://users.dimi.uniud.it/~massimo.franceschet/R/communities.html
c1 = cluster_fast_greedy(as.undirected(net2_sub))

# modularity measure
modularity(c1) #0.6130917

# modularity matrix
B = modularity_matrix(net2_sub, membership(c1))
round(B[1,],2)


# memberships of nodes
membership(c1)

# number of communities
length(c1) #12

# size of communities
sizes(c1)

#1  2  3  4  5  6  7  8  9 10 11 12 
#25  6  5  5 42  8  4  3  3  3  3  3 

# crossing edges
crossing(c1, net2_sub)


#File Name: plot_5
# plot communities with shaded regions
plot(c1, net2_sub, layout=layout_nicely, edge.arrow.size=0.0000000000000001, vertex.size=4, vertex.label=NA)

#File Name: plot_6 with vertices names
V(net2_sub)$label <- V(net2_sub)$name
V(net2_sub)$label.cex = 0.5
plot(c1, net2_sub, layout=layout_nicely, edge.arrow.size=0.0000000000000001, vertex.size=6, vertex.label.size=1)

E(net2_sub)
# $`1`
# [1] "1"   "18"  "25"  "34"  "48"  "60"  "91"  "122" "149" "168" "170" "183" "185" "196" "198" "229"
# [17] "232" "233" "236" "237" "238" "239" "244" "251" "260"


# $`2`
# [1] "8"   "26"  "169" "192" "197" "255"

# $`3`
# [1] "86"  "92"  "108" "109" "110"


# $`4`
# [1] "63" "65" "66" "68" "70"


# $`5`
# [1] "3"   "9"   "10"  "17"  "20"  "33"  "35"  "84"  "89"  "98"  "102" "136" "137" "138" "144" "147"
# [17] "160" "166" "171" "179" "180" "187" "188" "201" "205" "207" "214" "218" "220" "223" "227" "240"
# [33] "241" "250" "257" "262" "264" "265" "267" "309" "315" "463"


# $`6`
# [1] "142" "163" "164" "182" "200" "245" "252" "254"

# $`7`
# [1] "4"   "56"  "194" "226"

# $`8`
# [1] "74" "75" "76"


# $`9`
# [1] "124" "125" "126"

# $`10`
# [1] "64" "67" "71"

# $`11`
# [1] "59"  "61"  "231"

# $`12`
# [1] "80"  "81"  "119"