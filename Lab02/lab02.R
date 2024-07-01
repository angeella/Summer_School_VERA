knitr::opts_chunk$set(echo = T, fig.align = "center", out.width = '80%', warning = F, message = F)

## install.packages("igraph")

library(igraph)

load("Datasets/russian_trolls.RData")

net = graph_from_edgelist(Y, directed = F) 

for(j in 1:ncol(X)){ 
  net = net %>% 
    set_vertex_attr(name = colnames(X)[j],
                    value = X[,j]) 
}

V(net)$name = V(net)$vertex.names 

summary(net)

ecount(net) 
vcount(net)

E(net) 
V(net)
list.vertex.attributes(net)

net = delete_vertices(net, which(igraph::degree(net) < 1))

D_net = igraph::degree(net)
sort(D_net,decreasing = T)[1:5]

par(mfrow=c(1,2))
hist(D_net,nclass = 100, main = "Degree distribution", xlab = "Degree")
hist(igraph::degree(net,normalized = T),nclass=100, main = "Degree distribution (normalized)", xlab = "Normalized degree")

edge_density(graph = net)

par(mfrow= c(1,1))
dd.net <- degree_distribution(net)
d <- 1:max(degree(net))-1
ind <- (dd.net != 0)
plot(d[ind], dd.net[ind], log="xy", col="blue",
xlab=c("Log-Degree"), ylab=c("Log-Intensity"),
main="Log-Log Degree Distribution")

a.nn.deg.net <- knn(net,V(net))$knn
plot(degree(net), a.nn.deg.net, log="xy",
col="goldenrod", xlab=c("Log Vertex Degree"),
ylab=c("Log Average Neighbor Degree"))

shortest_paths(graph = net,from = "aiden7757",to = "_nickluna_")$vpath

mean_distance(graph = net)

par(mfrow = c(1,1))
S = distances(graph = net)
image(S,col = gray.colors(n = diameter(net))) 
#The diameter of a graph is the length of the longest geodesic.

C_net = igraph::closeness(graph = net, normalized = TRUE)
B_net = igraph::betweenness(graph = net,normalized = TRUE) 

par(mfrow=c(1,2))
hist(C_net, breaks = 50)
hist(B_net, breaks = 50)

names(which.max(C_net))
names(which.min(C_net))

names(which.max(B_net))
names(which.min(B_net))

## net.sub <- delete_vertices(net, vertex.attributes(net)$accounttype != "left")
## 
## A <- as_adjacency_matrix(net.sub, sparse=FALSE)
## sna::gplot.target(A, sna::degree(A))

subg = igraph::components(graph = net)
subg$csize

net = igraph::delete_vertices(net, which(subg$membership != 1))
C_net = igraph::closeness(graph = net,normalized = T)
B_net = igraph::betweenness(graph = net,normalized = T)
D_net = igraph::degree(net,normalized = T)

sort(C_net,decreasing = T)[1:5]
sort(D_net,decreasing = T)[1:5]
sort(B_net,decreasing = T)[1:5]

par(mfrow=c(1,2))
hist(C_net, breaks = 50)
hist(B_net, breaks = 50)

sum(B_net > .1)

cor(cbind(B_net,D_net,C_net))

library(GGally)
ggpairs((data.frame(B_net,D_net,C_net))) + theme_bw()

eb <- edge.betweenness(net)
E(net)[order(eb, decreasing=T)[1:3]]

table(sapply(cliques(net), length))

cliques(net)[sapply(cliques(net), length) == 6][[1]]
cliques(net)[sapply(cliques(net), length) == 6][[2]]
cliques(net)[sapply(cliques(net), length) == 6][[3]]

table(sapply(maximal.cliques(net), length))

clique.number(net)

cores <- coreness(net)
hist(cores)

graph.density(net)

sub.net <- induced.subgraph(net, igraph::neighborhood(net, 1, which(V(net)$accounttype == "left"))[[2]])
graph.density(sub.net)
sub.net <- induced.subgraph(net, igraph::neighborhood(net, 1, which(V(net)$accounttype == "right"))[[1]])
graph.density(sub.net)

transitivity(net)

transitivity(net, "local", vids = which(V(net)$accounttype == "left")[2])

aType = vertex_attr(graph = net,name="accounttype")

table(aType)

require(stringr)
aType = str_to_title(aType)
aType = str_replace(aType,"Ukranian","Russian")
aType = str_replace(aType,"Commercial|Local","News")
aType = str_replace(aType,"Koch","Fear")
aType = str_replace(aType,"^$|Arabic|\\?", "Other")
table(aType)

require(tidyverse)
df_pl = data.frame(den=D_net, clos = C_net, bet = B_net, x=aType)
#gather(df_pl,"den","clos","bet")
df_pl %>% dplyr::filter(x %in% c("Right","Left","Russian")) %>%
gather(.,"den","clos","bet",key="stat",value = "value") %>%
ggplot(aes(value,after_stat(density),color=x)) + 
  geom_freqpoly(lwd=1)+ 
  facet_wrap(~stat,scales="free")+ 
  theme_bw()

modularity(net,factor(aType))
assortativity(net,factor(aType))

gr = cluster_louvain(graph = net)
#gr2 = cluster_fast_greedy(graph = net) #agglomerative hierarchical clustering algorithm

class(gr)

table(membership(gr))
assortativity(net,membership(gr))

modularity(gr)

net1 <- net
vertex_attr(net1)$name  <- NULL
gr1 = cluster_louvain(graph = net1)
plot(gr1, net1,col = NULL)

table(membership(gr), aType)

save(list = ls(), file = "lab02.RData")
