#attempt 2 at script for SNA 


rm(list = ls()) # clear the workspace
library(igraph)



#next step laod the awareness data
init_matrix <- read.csv(file.choose(),  header=TRUE, row.names=1, check.names=FALSE, na.strings = "")

matrix <- as.matrix(init_matrix)

#the general network with all connection init

awareness <- graph.adjacency(matrix, mode = "undirected", weighted = NULL)

plot(awareness)

#Each matrix for each attribute.

matrixa = matrix
matrixb = matrix
matrixc = matrix
matrixd = matrix

#we need to create a network of matrix A, with everyone knowing each other well 
#therefore we set <=3 to 0 and =>4 to 1.


matrixa[matrixa <= 3]= 0 
matrixa[matrixa >= 4]= 1

aware_agree <- graph.adjacency(matrixa, mode = "undirected", weighted = NULL)


#we use a function to try and get a better layout.

lkk1 <- layout_with_kk(aware_agree) 
plot(aware_agree, layout=lkk1)



#now I setup the "nodes" of the network which will change based on the various datasets given
#first i setup nodes based on the location

nodes_location <- read.csv(file.choose(), header=T, as.is=T)


V(aware_agree)$location = nodes_location$location


# visualization
col = c("orange", "red", "turquoise", "purple")


V(aware_agree)$color=col[nodes_location$location]


#preppinng the plot

set.seed(42)
lagree=layout_with_fr(aware_agree)



plot(aware_agree, layout=lagree ,vertex.label=NA, vertex.size=8)
legend(x=-1.5, y=-1.1, c("Paris","Frankfurt", "Warsaw", "Geneva"), pch=21,
       col="#777777", pt.bg=col, pt.cex=.8, cex=.3, bty="n", ncol=1)

#Trying the loction per group plot with nummers in the graph based on the location


lfr <- layout_with_fr(aware_agree) 


plot(aware_agree 
     ,layout = lkk1
     ,vertex.color=V(aware_agree)$color
     ,vertex.shape=V(aware_agree)$shape
     ,vertex.frame.color="black"
     ,vertex.label.cex= 1)
     legend(x=-1.8, y=-0.2, c("Paris","Frankfurt", "Warsaw", "Geneva"), pch=21,
            col="#777777", pt.bg=col, pt.cex=2, cex=.8, bty="n", ncol=1,y.intersp = 0.25, x.intersp = 0.25)
     

#seperation of the graphs into sub graphs 

#col = c("orange", "red", "green", "purple")




PS <- induced.subgraph(aware_agree, nodes_location$location==1)




FT <- induced.subgraph(aware_agree, nodes_location$location==2)



WW <- induced.subgraph(aware_agree, nodes_location$location==3)



GA <- induced.subgraph(aware_agree, nodes_location$location==4)





#STATISTICS
#density transitivty calc

density(PS)
density(FT)
density(WW)
density(GA)

transitivity(PS)
transitivity(FT)
transitivity(WW)
transitivity(GA)

diameter(PS)
diameter(FT)
diameter(WW)
diameter(GA)


vertex.connectivity(PS)
vertex.connectivity(FT)
vertex.connectivity(WW)
vertex.connectivity(GA)

articulation.points(PS)
articulation.points(FT)
articulation.points(WW)
articulation.points(GA)




#INTERACTIONS
E(PS)

#table

table(nodes_location$location)


edge_density(PS)
edge_density(FT)
edge_density(WW)
edge_density(GA)

edge_density(aware_agree)


diameter(aware_agree, directed = FALSE, unconnected = TRUE, weights = NULL)

sort(degree(PS))
sort(betweenness(PS))


sort(degree(FT))
sort(betweenness(FT))


sort(degree(WW))
sort(betweenness(WW))


sort(degree(GA))
sort(betweenness(GA))

plot(WW)
plot(FT)
plot(WW)
plot(GA)

####################################################################
#
#
#
#
#
#
#
####################################################################




#TENURE
#connect nodes of organization level attributes to them



rm(list = ls()) # clear the workspace

library(igraph)


#next step laod the awareness data
init_matrix <- read.csv(file.choose(),  header=TRUE, row.names=1, check.names=FALSE, na.strings = "")

matrix <- as.matrix(init_matrix)



matrixa = matrix
matrixb = matrix
matrixc = matrix
matrixd = matrix

#we need to create a network of matrix A, with everyone knowing each other well 
#therefore we set <=3 to 0 and =>4 to 1.


matrixa[matrixa <= 3]= 0 
matrixa[matrixa >= 4]= 1

aware_agree <- graph.adjacency(matrixa, mode = "undirected", weighted = NULL)


#we use a function to try and get a better layout.

lkk1 <- layout_with_kk(aware_agree)

nodes_tenure <- read.csv(file.choose(), header=T, as.is=T)


#connecting the tenure levels to the nodes



V(aware_agree)$tenure = nodes_tenure$tenure


# visualization
colA = c("pink", "green", "turquoise", "purple")


V(aware_agree)$color=colA[nodes_tenure$tenure]



plot(aware_agree 
     ,layout = lkk1
     ,vertex.color=V(aware_agree)$color
     ,vertex.shape=V(aware_agree)$shape
     ,vertex.frame.color="black"
     ,vertex.label.cex= 1)
     legend(x=-1.8, y=-0.2, c("1-12","13-36", "37-60", "61 +"), pch=21,
            col="#777777", pt.bg=colA, pt.cex=2, cex=.8, bty="n", ncol=1,y.intersp = 0.25, x.intersp = 0.25)







twelve <- induced.subgraph(aware_agree, nodes_tenure$tenure==1)



thirty6 <- induced.subgraph(aware_agree, nodes_tenure$tenure==2)



sixty <- induced.subgraph(aware_agree, nodes_tenure$tenure==3)



sixty1 <- induced.subgraph(aware_agree, nodes_tenure$tenure==4)

plot(twelve)
plot(thirty6)
plot(sixty)
plot(sixty1)


#####STATISTICS

vertex_connectivity(aware_agree, source = NULL, target = NULL, checks = TRUE)

vertex_connectivity(thirty6, source = NULL, target = NULL, checks = TRUE)


edge_density(twelve)
edge_density(thirty6)
edge_density(sixty)
edge_density(sixty1)

transitivity(twelve)
transitivity(thirty6)
transitivity(sixty)
transitivity(sixty1)

diameter(twelve)
diameter(thirty6)
diameter(sixty)
diameter(sixty1)


vertex.connectivity(twelve)
vertex.connectivity(thirty6)
vertex.connectivity(sixty)
vertex.connectivity(sixty1)

articulation.points(twelve)
articulation.points(thirty6)
articulation.points(sixty)
articulation.points(sixty1)

articulation.points(aware)

x4 <- cluster_leading_eigen(level_4)
x4

plot(x4, level_4)



##################################################################################
##################################################################################

##################################################################################
##################################################################################

##################################################################################
##################################################################################


#ORGINANIZATIONAL LEVEL



rm(list = ls()) # clear the workspace

library(igraph)


#next step laod the awareness data
init_matrix <- read.csv(file.choose(),  header=TRUE, row.names=1, check.names=FALSE, na.strings = "")

matrix <- as.matrix(init_matrix)



matrixa = matrix
matrixb = matrix
matrixc = matrix
matrixd = matrix

#we need to create a network of matrix A, with everyone knowing each other well 
#therefore we set <=3 to 0 and =>4 to 1.


matrixa[matrixa <= 3]= 0 
matrixa[matrixa >= 4]= 1

aware_agree <- graph.adjacency(matrixa, mode = "undirected", weighted = NULL)


#we use a function to try and get a better layout.

lkk1 <- layout_with_kk(aware_agree)

nodes_org <- read.csv(file.choose(), header=T, as.is=T)


#connecting the organization levels to the nodes

plot(aware_agree)

V(aware_agree)$level = nodes_org$level


# visualization
colB = c("orange", "red", "turquoise", "purple")


V(aware_agree)$color=colB[nodes_org$level]



plot(aware_agree 
     ,layout = lkk1
     ,vertex.color=V(aware_agree)$color
     ,vertex.shape=V(aware_agree)$shape
     ,vertex.frame.color="black"
     ,vertex.label.cex= 1)
     legend(x=-1.8, y=-0.2, c("Global Dept","Local Dept", "Project Lead", "Researcher"), pch=21,
       col="#777777", pt.bg=colB, pt.cex=2, cex=.8, bty="n", ncol=1,y.intersp = 0.25, x.intersp = 0.25)




level_1 <- induced.subgraph(aware_agree, nodes_org$level==1)



level_2 <- induced.subgraph(aware_agree, nodes_org$level==2)



level_3 <- induced.subgraph(aware_agree, nodes_org$level==3)



level_4 <- induced.subgraph(aware_agree, nodes_org$level==4)


llk4 = layout_with_kk(level_4)

plot(level_1)
plot(level_2)
plot(level_3)
plot(level_4,
     layout= llk4 )


#####STATISTICS

edge_density(aware_agree)
diameter(aware_agree)
vertex_connectivity(aware_agree, source = NULL, target = NULL, checks = TRUE)
articulation.points(aware_agree)

vertex_connectivity(thirty6, source = NULL, target = NULL, checks = TRUE)


edge_density(level_1)
edge_density(level_2)
edge_density(level_3)
edge_density(level_4)

transitivity(level_1)
transitivity(level_2)
transitivity(level_3)
transitivity(level_4)

diameter(level_1)
diameter(level_2)
diameter(level_3)
diameter(level_4)

vertex.connectivity(level_1)
vertex.connectivity(level_2)
vertex.connectivity(level_3)
vertex.connectivity(level_4)

articulation.points(level_1)
articulation.points(level_2)
articulation.points(level_3)
articulation.points(level_4)


x4 <- cluster_leading_eigen(level_4)
x4

plot(x4, level_4)

rm("X", "X4")
