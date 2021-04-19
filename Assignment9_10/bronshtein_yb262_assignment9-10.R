library(sna)
library(tidyverse)
library(igraph)
library(intergraph)
library(GGally)
#PROBLEM 1:
  
#  Generate an undirected random graph with 20 vertices and probability of a tie
#(forming an edge) equal 0.7. Calculate the mean degree from these graphs. 
#Call it MD20.
#Now, keeping the probability of a tie fixed at 0.7, repeat the above step with 
#30, 40, 50, 60, 70, 80, 90 and 100 vertices, by writing a simple R function. 
#In each case calculate the average mean degree. Denote them by MD30, ..., MD 100.
#Plot the average mean degrees against the number of vertices. 
#Is the plot result consistent with your expectation? Explain.
set.seed(1)
net <- rgraph(n = 20, tprob = 0.7, mode = "graph")
MD20 <- mean(degree(net, gmode = "graph"))


get_mean_degree <- function(num_vertices) {
  set.seed(1)
  net <- rgraph(n = num_vertices, tprob = 0.7, mode = "graph")
  return(mean(degree(net, gmode = "graph")))
}
mean_list <- NULL
for (i in seq(from = 30, to = 100, by = 10)) {
  mean_list <- c(mean_list, get_mean_degree(i))
}
mean_degrees <- c("MD30", "MD40", "MD50", "MD60", "MD70", "MD80", "MD90", "MD100")
graph_means_t <- tibble(MeanDegrees = mean_degrees, MeanValues = mean_list)

ggplot(data = graph_means_t, mapping = aes(x = MeanDegrees, y = MeanValues)) +
  geom_point() +
  geom_line()
#todo: fix labels

#The results indicate a linear relationship. As the number of vertices increase,
# the meanValues grows


#PROBLEM 2:
  
#1.Download the data frames "Dataset1-Media-Example-NODES.csv and 
#"Dataset1-Media-Example-EDGES.csv" from 
#https:// (Links to an external site.)net/network-visualization 
#(Links to an external site.), and create an igraph network object.
#2.Convert the igraph to a network object in the sna package
#3. Plot the network using, using separate colors for the nodes based on the 
#vertex attribute media.type and make the size of the nodes proportional to the 
#vertex attribute audience.size. [Hint: Use network::get.vertex.attribute]
#4. Calculate the mean degree and density of the network using appropriate 
#functions in the sna package.
nodes <- read_csv("C:\\Users\\Julia\\Downloads\\Dataset1-Media-Example-NODES.csv")
links <- read_csv("C:\\Users\\Julia\\Downloads\\Dataset1-Media-Example-EDGES.csv")
net_2 <- graph_from_data_frame(d=links, vertices = nodes, directed = TRUE) %>% #directed is default
  simplify(remove.multiple = F, remove.loops = T)

#2.Convert igraph to network object in the sna package
network_obj <- asNetwork(net_2)
#3. Plot the network using, using separate colors for the nodes based on the 
#vertex attribute media.type and make the size of the nodes proportional to the 
#vertex attribute audience.size. [Hint: Use network::get.vertex.attribute]

g_size <- network::get.vertex.attribute(network_obj, "audience.size")
g_color <- network::get.vertex.attribute(network_obj, "media.type")

ggnet2(network_obj, arrow.size = 5, arrow.gap = 0.02, size = g_size, 
       label = FALSE,  color = g_color, palette = "Set2")

#4. Calculate the mean degree and density of the network using appropriate 
#functions in the sna package.
density <- network.density(network_obj)
density

mean_degree <- mean(degree(net_2))
mean_degree

