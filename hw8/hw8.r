rm(list = ls())

setwd("/Users/michaeljoyce/hw-machinelearning2k15/hw8/") #Mike
#setwd("~/Documents/R/machinelearning/machinelearning2k15/hw8/") #Jack
#setwd("C:/Users/Tom/Dropbox/Booth/Machine Learning/machinelearning2k15/hw8/") #Tom


library(igraph)


data(karate, package = "igraphdata")

# nodes in faction 1 will be rectangles
# nodes in faction 2 will be circles
shapes = c('rectangle', 'circle')
faction_vertex_shape = get.vertex.attribute(karate, "Faction")
faction_vertex_shape[faction_vertex_shape == 1] = shapes[1]
faction_vertex_shape[faction_vertex_shape == 2] = shapes[2]
karate_layout = layout.davidson.harel(karate)



visualize_karate_communities <- function(communities){
  communities_name = deparse(substitute(communities))
  png(paste0(communities_name,".png"))
  par(pin = c(6,6))
  plot(communities, karate, 
       layout=karate_layout, 
       vertex.shape=faction_vertex_shape)
  title(communities_name)
  dev.off()
}


## we need to apply all the clustering methods
## we need to visualize the outputs
visualize_karate_communities(cl)
methods <- c("x","y")
for(method in methods){
  cl = 
  visualize_karate_communities(cl)
}


## Run these [community detetction] algorithms on Zachary’s karate club data 
## where we know the ground truth and investigate what communities you get out. 
## Visualize communities obtained by different algorithms. 
## Briefly comment on outputs of different algorithms and
## their ability to uncover the true communities.
imc <- infomap.community(g)
?communities
membership(imc)
communities(imc)



## Load the network wikipedia.gml provided. 
## It is in gml format, which can be imported into _igraph using the following command
## read.graph(“wikipedia.gml”, format=“gml”)
## The vertices of this network are wikipedia pages. 
## The label of each vertex is the title of the wikipedia page.
## Now use any community detection algorithm. 
## Do you think the communities found make sense? 
## You can use the vertex labels to check this.
## Note that some algorithms work only on undirected graphs. 
## Also note that some algorithms are too slow to be run on big networks. 
## Check the manual to see if you can run the community detection algorithm on the wikipedia graph.

w <- read.graph("wikipedia.gml", format="gml")
