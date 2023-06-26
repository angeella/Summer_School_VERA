
## data: conflicts between countries during the Cold War, 
#every five years from 1950 to 1985

rm(list=ls())
war = read.table('Additional_material/war.txt')
war = as.matrix(war)
str(war)

war = 1*(war <  0)

isSymmetric(war)
lattice::levelplot(war)

library(igraph)

net = graph_from_adjacency_matrix(war, mode = 'undirected', weighted = NULL, diag=F)

E(net)
V(net)

edge_density(net) #That is, of all the possible connections, 
#we observe approximately 5 out of 100.


zeros = which(degree(net) < 1)
length(zeros) #4
net = delete.vertices(net,zeros)

## degree distriution
degree(net)
dev.off()
hist(degree(net),breaks = 30,col='lavender')

#?shortest_paths
shortest_paths(net, from = 'ITA')

#other measures
hist(sqrt(betweenness(net)), breaks = 20, col = 'lavender')

diameter(net)

plot(net)




set.seed(1)
net_lo = layout_with_graphopt(net, niter =3500)
plot(net,layout= net_lo)

### Improve removing disconnetted
rem = c('HAI', 'DOM', 'ARG', 'CHL') 
net = delete.vertices(net, rem)

set.seed(1)
net_lo = layout_with_graphopt(net, niter =3500)

###Again
V(net)$size = degree(net)+1
plot(net, layout= net_lo)

V(net)$size = sqrt(betweenness(net))
plot(net, layout= net_lo)


comm = multilevel.community(net)#This function implements the 
#multi-level modularity optimization algorithm for 
#finding community structure
comm2 = cluster_fast_greedy(net)



gr = membership(comm)
gr2 = membership(comm2)

modularity(net,gr)
modularity(net,gr2)

assortativity(net,gr)
assortativity(net,gr2)

##numbers of clusters
max(gr2)

set.seed(1)
net_col = as.vector(gr2)

net_col


V(net)$color = net_col
plot(net, layout= net_lo)


V(net)$label.color = 'white'
V(net)$label.font = 2

V(net)$size = pmin(sqrt(betweenness(net)), 10)+2
par(bg='black')
plot(net, layout= net_lo)


#+++++
# ERGM
#+++++

detach(package:igraph)
library(ergm)

#remove empty vertex

rem_id = which(colnames(war) %in% rem)
war_red = war[-c(rem_id, zeros), -c(rem_id, zeros)]

net2 = as.network(war_red, directed=F) #network objects from network package

par(bg='white')
plot(net2)
summary(net2)


#We begin with a simple model, containing only one term that 
#represents the total number of edges in the network
summary(net2~edges) # how many edges?
m0 = ergm(net2~edges, estimate = 'MPLE')#maximum pseudolikelihood estimator 
#is returned
summary(m0)

#This simple model specifies a single homogeneous probability for all ties, 
#which is captured by the coefficient of the edges term. How should we interpret 
#this coefficient? 

#The corresponding probability is obtained by the inverse logit, 
#of our estimated parameter:
exp(m0$coef)/(1+exp(m0$coef))
plogis(m0$coef)
##TRIVIALLY, OBSERVING AN ARC INCREASES THE PROBABILITY OF 0.06594071 
#to have a tie

#Let’s add a term often thought to be a measure of “clustering”: 
#the number of completed triangles in the network
#Triangles measure transitivity and clustering in networks.
m1 = ergm(net2~edges+triangles, estimate = 'MPLE')
summary(m1)

ergmMPLE(net2~edges+triangles) #ERGM Predictors and response for logistic regression calculation of MPLE
matr = ergmMPLE(net2~edges+triangles)

##### interpretation
### if an arc carries no triangle
plogis(coef(m1)[1])

## If a node carries a triangle
plogis(c(1,1) %*% coef(m1))

## If a node carries two triangles
plogis(c(1,2) %*% coef(m1))

###GOF
# gof simulates networks from the ERGM estimates and, 
#for some set of network statistics, compares the distribution 
#in the simulated networks to the observed values.
#A low p-value suggests that there may be a problem with the 
#fit for that graph statistic. A particularly useful feature of gof() 
#is the ability to generate box plots of the simulated counts and overlay 
#your observed graph statistics. 
#This can provide a quick sanity check of the quality of your 
#model and can help you formulate hypotheses about why your model 
#might be failing.

gof_m1 = gof(m1)#Conduct Goodness-of-Fit Diagnostics on a Exponential Family Random Graph Model
gof_m1
plot(gof(m1~degree))
plot(gof(m1~distance))
plot(gof(m1~triadcensus))


#We often have a situation where we think that the attributes of the 
#individuals who make up our graph vertices may affect their propensity 
#to form (or receive) ties. To test this hypothesis, we can employ nodal 
#covariates using the nodecov() term.

################################################################################
###################################REFERENCES###################################
################################################################################

#https://cran.r-project.org/web/packages/ergm/ergm.pdf
#https://gvegayon.github.io/appliedsnar/exponential-random-graph-models.html
#https://eehh-stanford.github.io/SNA-workshop/ergm-intro.html#getting-started

####################Other References####################
##Graphical VAR model:
#https://cran.r-project.org/web/packages/BGGM/vignettes/var_model.html
## SVAR:
#https://cran.r-project.org/web/packages/svars/vignettes/svars.pdf






