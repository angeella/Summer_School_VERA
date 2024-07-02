## ----setup, include=FALSE-------------------------------------------
knitr::opts_chunk$set(echo = T, fig.align = "center", out.width = '80%', warning = F, message = F)
rm(list = ls())
#load("Lab02/lab02.RData")


## -------------------------------------------------------------------
load("../Lab02/lab02.RData")


## -------------------------------------------------------------------
library(igraph)
library(ggraph)


## -------------------------------------------------------------------
par(mfrow = c(1,1))
plot(net)


## -------------------------------------------------------------------
V(net)$name[which(B_net < quantile(B_net, .99))] = NA
plot(net, 
     vertex.label=V(net)$name, 
     vertex.label.color = "black",
     edge.color="grey", 
     vertex.color="orange", 
     vertex.frame.color="#ffffff",
     main = "3 million Russian troll tweets")


## -------------------------------------------------------------------
set.seed(1234)
plot(net, 
     vertex.size = log(igraph::betweenness(net)+1),
     vertex.color = as.numeric(factor(aType)),
     vertex.label=V(net)$name, 
     vertex.label.color = "black",
     edge.color="grey", 
     vertex.color="orange", 
     vertex.frame.color="#ffffff",
     main = "3 million Russian troll tweets")


## -------------------------------------------------------------------
V(net)$label = V(net)$name
V(net)$label[which(B_net < quantile(B_net, .99))] = NA
V(net)$label.color = "black"
V(net)$size = log(igraph::betweenness(net)+1)
V(net)$color = as.numeric(factor(aType))
E(net)$arrow.size<-.2 
E(net)$edge.color<-"gray80"


## -------------------------------------------------------------------
set.seed(1234)
plot(net)


## -------------------------------------------------------------------
llMDS = layout_with_mds(net)
plot(net,layout= llMDS)


## -------------------------------------------------------------------
set.seed(1)
llFR = layout_with_fr(net,niter = 5000)
plot(net,layout= llFR)


## -------------------------------------------------------------------
par(mfrow=c(1,2), mar=c(0,0,0,0)) # plot four figures- 1 rows, 2 columns 
plot(net, layout=layout_with_fr)
plot(net, layout=llFR) 


## -------------------------------------------------------------------
par(mfrow= c(1,1))
set.seed(1)
llKK = layout_with_kk(net,dim = 2,maxiter = 100*vcount(net))
plot(net,layout= llKK)


## -------------------------------------------------------------------
l <- layout_with_graphopt(net) 
plot(net, layout=l)


## -------------------------------------------------------------------
l1 <- layout_with_graphopt(net, charge=0.001) 
l2 <- layout_with_graphopt(net, charge=0.00000001) 
par(mfrow=c(1,2), mar=c(1,1,1,1)) 
plot(net, layout=l1) 
plot(net, layout=l2)


## -------------------------------------------------------------------
par(mfrow=c(1,1))
graph_attr(net,"layout")<-layout_with_lgl 
plot(net)


## -------------------------------------------------------------------
layouts <- grep("layout_", ls("package:igraph"), value=TRUE)[-c(1:2)]

layouts <- layouts[grepl("attr|graphopt|with_kk|with_fr|with_mds", layouts)] 
par(mfrow=c(2,2), mar=c(1,1,1,1)) 
for(layout in layouts){ 
  print(layout) 
  l <- do.call(layout, list(net)) 
  plot(net, edge.arrow.mode=0, layout=l, main=layout) 
}


## -------------------------------------------------------------------
V(net)$community <- gr$membership 
colrs <- adjustcolor(sample(colors(distinct = TRUE),11), alpha=.6) 
plot(net, 
     vertex.color=colrs[V(net)$community],
     vertex.label = NA)


## -------------------------------------------------------------------
right.path<-shortest_paths(net,
                        from=V(net)[aType=="Left"],
                        to=V(net)[aType=="Right"], 
                        output="both")

ecol<-rep("gray80",ecount(net)) 
ecol[unlist(right.path$epath)]<-"orange" #color edges
ew<-rep(2,ecount(net)) 
ew[unlist(right.path$epath)]<-4  #size
vcol<-rep("gray40",vcount(net)) 
vcol[unlist(right.path$vpath)]<-"gold" #color vertices
plot(net,
     vertex.color=vcol,
     edge.color=ecol, 
     edge.width=ew,edge.arrow.mode=0, layout = layout_with_graphopt)


## -------------------------------------------------------------------
inc.edges <- incident(net, V(net)[aType=="Left"], mode="all") #mode argument ignored for undirected graphs
ecol <- rep("gray80", ecount(net)) 
ecol[inc.edges] <- "orange" 
vcol <- rep("grey40", vcount(net)) 
vcol[V(net)[aType=="News"]] <- "gold" 
plot(net, 
     vertex.color=vcol, 
     edge.color=ecol, layout = layout_with_graphopt)


## -------------------------------------------------------------------
neigh.nodes <- neighbors(net, V(net)[aType=="Left"], mode="out")
vcol[neigh.nodes] <- "#ff9d00" 
plot(net, vertex.color=vcol, layout = layout_with_graphopt)


## -------------------------------------------------------------------
par(mfrow=c(1,2)) 
plot(net, mark.groups=which(membership(gr)==1), mark.col="#C5E5E7", mark.border=NA, layout = layout_with_graphopt)
# Mark multiple groups: 
plot(net, mark.groups=list(which(membership(gr)==1), which(membership(gr)==2)), mark.col=c("#C5E5E7","#ECD89A"), mark.border=NA, layout = layout_with_graphopt)


## ----eval = FALSE---------------------------------------------------
## #do not run!
## tkid <- tkplot(net) #tkid is the id of the tkplot that will open
## l <- tkplot.getcoords(tkid) # grab the coordinates from tkplot
## plot(net, layout=l)


## -------------------------------------------------------------------
library(ggraph) 
ggraph(net) + 
  geom_edge_link(color = "gray70") + 
  geom_node_point(color = V(net)$color) + 
  theme_minimal()


## -------------------------------------------------------------------
library(tidygraph)
df = as_tbl_graph(net)
df %>% mutate(btw= .5*log(centrality_betweenness()+1)) %>%
ggraph(layout="fr") + 
  geom_edge_link(show.legend = FALSE, color="gray70") +
  geom_node_point(aes(colour = aType, size= btw), show.legend=T) + 
  geom_node_label(aes(label=label))+
  scale_size(guide="none") + 
  theme_graph(base_family='sans') +
  theme(legend.position = "bottom")


## -------------------------------------------------------------------
groupL = as.character(membership(gr))
table(groupL)
groupL[groupL %in% c(10,7,9, 11, 4)] = 4
groupL[groupL %in% c(8)] = 7


## -------------------------------------------------------------------
library(stringr)
table(groupL, aType)
groupL = str_replace(groupL, "1$", "Russian")
groupL = str_replace(groupL, "2", "Right")
groupL = str_replace(groupL, "3", "Russian2")
groupL = str_replace(groupL, "4", "Other")
groupL = str_replace(groupL, "5", "Left")
groupL = str_replace(groupL, "6", "Hashtager")
groupL = str_replace(groupL, "7", "German")


## -------------------------------------------------------------------
assortativity(net, as.factor(groupL))


## -------------------------------------------------------------------
df = as_tbl_graph(net)
df %>% mutate(btw= centrality_betweenness()) %>%
ggraph(layout="fr") + 
  geom_edge_link(show.legend = FALSE,color="gray") +
  geom_node_point(aes(colour = groupL,size= btw),alpha =
1,show.legend=T) + 
  geom_node_label(aes(label=label))+
  theme_graph(base_family='sans') + 
  scale_size(guide="none") +
  theme(legend.position = "bottom")


## -------------------------------------------------------------------
#load("../Lab02/lab02.RData")
V(net)$better_gr = groupL

df = as_tbl_graph(net, directed = FALSE)
df = df %>% activate(edges) %>% 
  mutate(e_col = ifelse(.N()$better_gr[to]
== .N()$better_gr[from], .N()$better_gr[from], NA))

df %>% 
  activate(nodes) %>% 
  mutate(btw= centrality_betweenness()) %>% 
  ggraph(layout="fr") +
  geom_edge_link(aes(color=e_col),alpha=.7,show.legend = FALSE) +
  geom_node_point(aes(colour = groupL,size= btw),show.legend=T) + 
  geom_node_label(aes(label=label))+
  theme_graph(base_family='sans') + 
  theme(legend.position = "bottom") +
  scale_size(guide="none") + 
  scale_edge_color_discrete(guide="none")

