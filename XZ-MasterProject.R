setwd("~/Downloads/UniLeeds/Dissertation/R code")

#Xitao Zhou 201289445
#Dissertation
#Topic 56 Network
#R code
#DRAFT
library(RColorBrewer)
#install.packages("igraph")
library("igraph", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
#demo(package="igraph")
library("statnet", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("network", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("intergraph", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("cluster", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("devtools", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
#install.packages("igraphdata")
#install.packages("statnet")
#install.packages("intergraph")
#install.packages("devtools")
install_github("ValeriaPolicastro/robin",force=TRUE)
library("robin")
library("gprege")

#igraphdata
rm(list = ls())
data(package = "igraphdata")
library("igraphdata", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
data("UKfaculty")
summary(UKfaculty)
data("enron")
summary(enron)



# select colors
colors = brewer.pal(4, "Dark2")
# assign colors to groups
V(UKfaculty)$color = sapply(V(UKfaculty)$Group, function(x) colors[x])

#Convert the directed network into undirected network
UKfaculty_undirected <- as.undirected(
  UKfaculty, 
  mode = "mutual",                              # keeping only mutual ties
  edge.attr.comb = list(weight="sum", "ignore") # sum edge weights, ignore other attributes
)

#check if the new network is simple:
is.simple(UKfaculty_undirected)

plot(UKfaculty_undirected, layout = layout_nicely(UKfaculty_undirected, dim = 2),
     vertex.color = V(UKfaculty)$color, vertex.frame.color = NA,
     vertex.label = NA, vertex.shape = 'square',
     vertex.size = 5, edge.curved = TRUE,
     edge.width = 2)
title("UK Faculty Friendship Network (UNdirected)", cex.main = 1)


#degree(UKfaculty_undirected)
vcount(UKfaculty_undirected)
ecount(UKfaculty_undirected)
#edge_density(UKfaculty_undirected)
#same result as ecount()/vcount()*(vcount()-1)

#Read in data
#data = read.csv(file.choose(), header = T)

#Degree Distribution
#V(UKfaculty_undirected)$degree = degree(UKfaculty_undirected)
#hist(V(UKfaculty_undirected)$degree, col="grey", 
#     main = "Histogram of Node Degree", ylab = "Number of nodes", xlab = "Node Degrees")
#if we use vertex.size = V(..)$degree*0.4 will spcify the nodes with higher degree
#Can be used in Supernode part
#in plot, layout = layout...., can change the picture, may give better picture

#In directed network, Authorities = incoming links, can use this to plot the 
#network to identify supernodes
#R code: hub_score(network)$vector
#        authority.score(network)$vector

#Community Detection
#Community detection based on edge betweenness (Newman-Girvan)
start_time_GN = Sys.time()
clustered_UKfaculty_undirected_1 = cluster_edge_betweenness(UKfaculty_undirected)
end_time_GN = Sys.time()
rtime_GN = end_time_GN - start_time_GN
rtime_GN

plot(clustered_UKfaculty_undirected_1, UKfaculty_undirected,
     vertex.frame.color = NA,
     vertex.label = NA, vertex.shape = 'square',
     vertex.size = 4, layout=layout.auto)
title("Clustered undirected Friendship Network(Girvan-Newman Algorithm)",
      cex.main = 0.8)

dendPlot(clustered_UKfaculty_undirected_1, mode="hclust") 
title(xlab = "Node Number", ylab = "Height",
      main = "Dendrogram of Clustering Process on Friendship Network",
      cex.main = 1)

#Another way to produce Dendrogram
yh = as.hclust(clustered_UKfaculty_undirected_1,hang = -1, use.modularity = FALSE)
plot(yh,main = "Dendrogram of Clustering Process on Friendship Network")


#examine the community detection igraph object
class(clustered_UKfaculty_undirected_1)
length(clustered_UKfaculty_undirected_1) #number of communities
membership(clustered_UKfaculty_undirected_1) # community membership for each node
modularity(clustered_UKfaculty_undirected_1) # how modular the graph partitioning is,????
crossing(clustered_UKfaculty_undirected_1, UKfaculty_undirected)   # boolean vector: TRUE for edges across communities

#Producedure to validate the robustness for GIrvan-Newman Algorithm
gRandom_GN = random(graph = UKfaculty_undirected)
gRandom_GN
#vi =  variation of information
proc_GN = robinRobust(graph = UKfaculty_undirected,
                      graphRandom = gRandom_GN,
                      measure = "vi",
                      method = "edgeBetweenness",
                      type = "independent")
#Robin plot for Girvan-Newman Algorithm
plotRobin(graph = UKfaculty_undirected,
          model1 = proc_GN$Mean, model2 = proc_GN$MeanRandom,
          legend = c("real data","null model"), measure = "vi",
          title = "Robin Plot for Girvan-Newman Algorithm")

#Interval-wise Testing for Girvan-Newman Algorithm
robinFDATest(graph = UKfaculty_undirected,
             model1 = proc_GN$Mean, model2 = proc_GN$MeanRandom,
             measure = "vi")

#Gaussian Process Approach for GN algorithm
#compute the corresponding Bayes Factor
robinGPTest(ratio = proc_GN$ratios)


#Community detection based on based on Louvain Algorithm
start_time_louvain = Sys.time()
clustered_UKfaculty_undirected_2 = cluster_louvain(UKfaculty_undirected)
end_time_louvain = Sys.time()
rtime_louvain = end_time_louvain - start_time_louvain #running time of louvain alg.
plot(clustered_UKfaculty_undirected_2, UKfaculty_undirected,
     vertex.frame.color = NA,
     vertex.label = NA, vertex.shape = 'square',
     vertex.size = 4)
title("Clustered undirected Friendship Network(Louvain Algorithm)",
      cex.main = 1,layout=layout.auto)
length(clustered_UKfaculty_undirected_2) #number of communities
a = membership(clustered_UKfaculty_undirected_2) # community membership for each node
#find members in each community
which(a == 1)
which(a == 2)
which(a == 3)
which(a == 4)
which(a == 5)
which(a == 6)
which(a == 7)
which(a == 8)

modularity(clustered_UKfaculty_undirected_2) # how modular the graph partitioning is,????

#Producedure to validate the robustness for Louvain Algorithm
gRandom_L = random(graph = UKfaculty_undirected)
gRandom_L
#vi =  variation of information
proc_L = robinRobust(graph = UKfaculty_undirected,
                      graphRandom = gRandom_L,
                      measure = "vi",
                      method = "louvain",
                      type = "independent")
#Robin plot for Louvain Algorithm
plotRobin(graph = UKfaculty_undirected,
          model1 = proc_L$Mean, model2 = proc_L$MeanRandom,
          legend = c("real data","null model"), measure = "vi",
          title = "Robin Plot for Louvain Algorithm")

#Interval-wise Testing for Louvain Algorithm
robinFDATest(graph = UKfaculty_undirected,
             model1 = proc_L$Mean, model2 = proc_L$MeanRandom,
             measure = "vi")

#Gaussian Process Approach for GN algorithm
#compute the corresponding Bayes Factor
robinGPTest(ratio = proc_L$ratios)

#Comparison btw GN algorithm and Louvain Algorithm
comp = robinCompare(graph = UKfaculty_undirected,
                    method1 = "edgeBetweenness",method2 = "louvain",
                    measure = "vi", type = "independent")

#plot the two VI curves for GN and Louvain alg
plotRobin(graph = UKfaculty_undirected,
          model1 = comp$Mean1, model2 = comp$Mean2,
          measure = "vi", legend = c("Girvan-Newman Algorithm","Louvain Algorithm"),
          title = "Girvan-Newman vs Louvain")


#Read in data from Italian Politics Dataset
Cosponsorship_Edgelist = read.table("graphCameraEdgeList.txt",header = TRUE)
Cosponsorship_Gender = read.table("cameraGenderMetadata.txt", header = TRUE)
Cosponsorship_Parties = read.table("cameraPartyMetadata.txt", header = TRUE)
#IDlist is a character vector
Cosponsorship_Node_IDlist = as.data.frame(Cosponsorship_Gender$id,stringsAsFactors=FALSE)

#Create the network as igraph variable 
Cosponsorship_network = graph_from_data_frame(d = Cosponsorship_Edgelist, 
                                              vertices = Cosponsorship_Node_IDlist,
                                              directed = FALSE)
class(Cosponsorship_network)

#Add metadata
# Create the variable
#gender <- c(rep("Female",num_nodes/2),rep("Male",num_nodes/2))
# Add it to the network object
vertex_attr(Cosponsorship_network, # the name of the network object
            index = V(Cosponsorship_network),
            # the name we want to reference the variable by in that object
            name = "Gender")  = as.character(Cosponsorship_Gender$label)# the value we are giving that variable
#check whether the Matadata is added successfully
vertex_attr(Cosponsorship_network,"Gender")

vertex_attr(Cosponsorship_network,name = "Parties")= as.character(Cosponsorship_Parties$label)

#All three attributes, "name","Gender","Parties" are added successfully
vertex_attr_names(Cosponsorship_network)

#Plot the Graph
#color by genders, gorder() gives Number of vertices, numeric scalar.
#numberofnodes = gorder(Cosponsorship_network)
#assign the "Sex" attribute as the vertex color
V(Cosponsorship_network)$color = V(Cosponsorship_network)$Gender 
V(Cosponsorship_network)$color = gsub("female", "pink", 
                                      V(Cosponsorship_network)$color)
V(Cosponsorship_network)$color = gsub("male", "lightblue", 
                                      V(Cosponsorship_network)$color)


plot(Cosponsorship_network, # our igraph object
     vertex.size = 2, # node size
     vertex.label = NA, # not show the node names
     layout= layout.auto
)
legend(x=-1.5, y=-1.1, c("female","male"), pch=21,
       col="#777777", pt.bg=c("Pink","Lightblue"), pt.cex=2, cex=.8, bty="n", ncol=1)
title("Original Bill Cosponsorship Network")

#Clustering
#EB_clustered_Cosponsorship_network = cluster_edge_betweenness(Cosponsorship_network)
#plot(clustered_Cosponsorship_network, Consponsored_network, vertex.size = 3, vertex.label = NA)
#title("Clustered Bill Cosponsorship Network(Girvan-Newman Alogrithm)")
#Infinite time, too costy, not efficient

start_BC_louvain = Sys.time()
L_clustered_Cosponsorship_network = cluster_louvain(Cosponsorship_network)
end_BC_louvain = Sys.time()
#running time of louvain alg.
rtime_BC_louvain = end_BC_louvain - start_BC_louvain
rtime_BC_louvain

plot(L_clustered_Cosponsorship_network, Cosponsorship_network, vertex.size = 3, vertex.label = NA,
     layout = layout.auto, vertex.color = V(Cosponsorship_network)$color)

title("Clustered Bill Cosponsorship Network(Louvain Alogrithm)")
#modularity of the final community assignment
modularity(L_clustered_Cosponsorship_network)
#Number of communities
length(L_clustered_Cosponsorship_network)

#Robin plots
#Producedure to validate the robustness
gRandom_L_BC = random(graph = Cosponsorship_network)
gRandom_L_BC
#vi =  variation of information
proc_L_BC = robinRobust(graph = Cosponsorship_network,
                      graphRandom = gRandom_L_BC,
                      measure = "vi",
                      method = "louvain",
                      type = "independent")
#Robin plot
plotRobin(graph = Cosponsorship_network,
          model1 = proc_L_BC$Mean, model2 = proc_L_BC$MeanRandom,
          legend = c("real data","null model"), measure = "vi",
          title = "Robin Plot for Louvain Algorithm on BC Network")

#Interval-wise Testing
robinFDATest(graph = Cosponsorship_network,
             model1 = proc_L_BC$Mean, model2 = proc_L_BC$MeanRandom,
             measure = "vi")

#Gaussian Process Approach
#compute the corresponding Bayes Factor
robinGPTest(ratio = proc_L_BC$ratios)

#Check how many parties are there in the network
#8 parties in total
a = vertex_attr(Cosponsorship_network, name = "Parties")
a = unique(a)
length(a)

#Relationship btw the community structure and the metadata
#color by the metadata, shapes by the memberships
#numberofnodes = gorder(Cosponsorship_network)

#Color by parties


#For Parties
for(i in 1:663){
  if (V(Cosponsorship_network)$Parties[i] == 'FI'){
    V(Cosponsorship_network)$color[i] = 1
  } else if (V(Cosponsorship_network)$Parties[i]=='PD'){
    V(Cosponsorship_network)$color[i] = 2
  } else if (V(Cosponsorship_network)$Parties[i]=='LN'){
    V(Cosponsorship_network)$color[i] = 3
  } else if (V(Cosponsorship_network)$Parties[i]=='FLI'){
    V(Cosponsorship_network)$color[i] = 4
  } else if (V(Cosponsorship_network)$Parties[i]=='UDC'){
    V(Cosponsorship_network)$color[i] = 5
  } else if (V(Cosponsorship_network)$Parties[i]=='mixed'){
    V(Cosponsorship_network)$color[i] = 6
  } else if (V(Cosponsorship_network)$Parties[i]=='IdV'){
    V(Cosponsorship_network)$color[i] = 7
  } else {
    V(Cosponsorship_network)$color[i] = 8
  }
}


V(Cosponsorship_network)$community = membership(L_clustered_Cosponsorship_network)
V(Cosponsorship_network)[V(Cosponsorship_network)$community == 1]$shape = 'square'
V(Cosponsorship_network)[V(Cosponsorship_network)$community == 2]$shape = 'circle'
V(Cosponsorship_network)[V(Cosponsorship_network)$community == 3]$shape = 'pie'
V(Cosponsorship_network)[V(Cosponsorship_network)$community == 4]$shape = 'vrectangle'
plot(Cosponsorship_network, vertex.label = NA,
     vertex.size = 2, # node size
     layout= layout.auto)
title(main = "Bill Cosponsorship Network")

#Super-node
source('SuperNode.R')
#Top 10% Super-node Network
#number of nodes in the compressed network
S = round(0.1*vcount(Cosponsorship_network))
#select seeds
SNOut_10 = SuperNode(Cosponsorship_network,S)
#construct the compressed network
#use igraph::degree instead of degree
Node2SN_10 =TurnToUniqueLab(SNOut_10$SNAssn)
Super_Network_10 = SNOut_10$SNNet #new compressed network

plot(Super_Network_10, vertex.label = NA,vertex.size = 5,
     edge.width = 0.3, layout = layout.auto)
title('Top 10% Super-node Network')

start_L_SN10 = Sys.time()
L_SN10 = cluster_louvain(Super_Network_10)
end_L_SN10 = Sys.time()
#running time of louvain alg.
rtime_L_SN10 = end_L_SN10 - start_L_SN10
rtime_L_SN10

plot( L_SN10, Super_Network_10,
     vertex.label = NA,vertex.size = 5, 
     edge.width = 0.3, layout = layout.auto)
title('Clustered Top 10% Super-node Network')
length(L_SN10)
modularity(L_SN10)

#top 30% supernode network
S3 = round(0.3*vcount(Cosponsorship_network))
#select seeds
SNOut_30 = SuperNode(Cosponsorship_network,S3)
#construct the compressed network
#use igraph::degree instead of degree
Node2SN_30 =TurnToUniqueLab(SNOut_30$SNAssn)
Super_Network_30 = SNOut_30$SNNet #new compressed network

plot(Super_Network_30, vertex.label = NA,vertex.size = 5,
     edge.width = 0.2, layout = layout.auto)
title('Top 30% Super-node Network')


start_L_SN30 = Sys.time()
L_SN30 = cluster_louvain(Super_Network_30)
end_L_SN30 = Sys.time()
#running time of louvain alg.
rtime_L_SN30 = end_L_SN30 - start_L_SN30
rtime_L_SN30

plot( L_SN30, Super_Network_30,
      vertex.label = NA,vertex.size = 5, 
      edge.width = 0.2, layout = layout.auto)
title('Clustered Top 30% Super-node Network')
length(L_SN30)
modularity(L_SN30)
