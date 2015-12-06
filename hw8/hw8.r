rm(list = ls())
set.seed(69)

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



visualize_karate_communities <- function(communities, communities_name){
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
functions <- c('cluster_fast_greedy'
               ,'cluster_infomap'
               ,'cluster_label_prop'
               ,'cluster_leading_eigen'
               ,'cluster_louvain'
               ,'cluster_optimal'
               ,'cluster_spinglass'
               ,'cluster_walktrap')

for(function_name in functions){
  print(function_name)
  
  cl_method = match.fun(function_name)
  cl = cl_method(karate)
  
  print(paste(length(unique(cl$membership)), "communities detected"))
  
  if(is.hierarchical(cl)){
     print("trying to cut")
     cut_cl = cutat(cl, no=2)
     cl$membership=cut_cl
  }

  print(paste(length(unique(cl$membership)), "communities now used"))
  
  visualize_karate_communities(cl, function_name)
}

?cluster_louvain

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
library(dplyr)
library(ggplot2)

w <- read.graph("wikipedia.gml", format="gml")
cl_cluster_walktrap <- cluster_walktrap(w)
cldf <- data.frame(label = get.vertex.attribute(w, "label"),
                   community = cl_cluster_walktrap$membership)
counts <- cldf %>%
  group_by(community) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

head(counts)
ggplot(data = counts, aes(x = count)) + geom_bar() + xlab("Number of pages in a community") + 
  ggtitle("Distribution of community sizes")
ggsave("Distribution of Wiki community sizes.png", 4, 4)

cldf %>% filter(community == 5) %>% head(n = 10) %>% select(label) %>% mutate(label = as.character(label)) %>% as.vector()
cldf %>% filter(community == 821) %>% head(n = 10) %>% select(label) %>% mutate(label = as.character(label)) %>% as.vector()

head(counts)

for(id in head(counts, 5)$community){
  print(id)  
}

sample_community_ids <- cldf$community %>% unique %>% sample(size = 5)

for(id in sample_community_ids){
  this_community <- cldf %>% filter(community == id)
  print(paste0("community ",id,":"))
  
  if(nrow(this_community) > 5){
    this_community
    

  } else {
    print(as.character(this_community$label))
  }
}


print(paste(length(unique(cl$membership)), "communities now used"))




head(V(w))
