## ----setup, include=FALSE-------------------------------------------
knitr::opts_chunk$set(echo = T, fig.align = "center", out.width = '80%', warning = F, message = F)


## -------------------------------------------------------------------
rm(list = ls())
library(statnet)
library(sand) #to load the lazega data
library(ergm)
data(lazega)


## -------------------------------------------------------------------
library(intergraph)
lazega.ergm <- asNetwork(lazega)
lazega.ergm


## -------------------------------------------------------------------
v.names <- vertex_attr_names(lazega)
sapply(v.names, function(x) table(vertex_attr(lazega, x)))


## -------------------------------------------------------------------
years <- lazega.ergm %v% 'Years' 
# %v% references vertex attributes, 
#equivalent to get.vertex.attribute(lazega.ergm, "Years")
#from igraph.

plot(lazega.ergm, 
     vertex.col = "tomato", 
     vertex.cex = years/10)


## ----eval = FALSE---------------------------------------------------
## ?ergm.terms


## -------------------------------------------------------------------
summary(lazega.ergm ~ edges) 


## -------------------------------------------------------------------
random_graph <- ergm(lazega.ergm ~ edges)


## -------------------------------------------------------------------
theta <- coef(random_graph)
theta


## -------------------------------------------------------------------
inv.logit <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

inv.logit(theta)


## -------------------------------------------------------------------
network.density(lazega.ergm)


## -------------------------------------------------------------------
summary(random_graph)


## -------------------------------------------------------------------
set.seed(1234)
hundred_simulations <- simulate(random_graph, 
                  coef = theta,
                  nsim = 100,
                  control = control.simulate.ergm(MCMC.burnin = 1000, #number of proposals before any MCMC sampling is done.
                                                  MCMC.interval = 1000)) #Number of proposals between sampled statistics


## -------------------------------------------------------------------
par(mfrow = c(1, 3))
sapply(hundred_simulations[1:3], plot, vertex.cex = 3, vertex.col = "tomato")


## -------------------------------------------------------------------
net_densities <- unlist(lapply(hundred_simulations, network.density))

hist(net_densities, xlab = "Density", main = "", col = "lightgray")
abline(v = network.density(lazega.ergm), col = "red", lwd = 3, lty = 2)
abline(v = mean(net_densities), col = "blue", lwd = 3, lty = 1)


## -------------------------------------------------------------------
gof_stats <- gof(random_graph,coef = theta)

par(mfrow = c(2, 2))
plot(gof_stats, main = '')


## -------------------------------------------------------------------
summary(lazega.ergm~edges+triangle) # 


## ----eval = FALSE---------------------------------------------------
## tr_graph <- ergm(lazega.ergm ~ edges + triangles)


## -------------------------------------------------------------------
summary(lazega.ergm ~ edges + gwesp(log(3), fixed=TRUE)
 + nodecov("Practice")
 + nodematch("Practice")
 + nodematch("Gender")
 + nodematch("Office"))


## -------------------------------------------------------------------
lazega.ergm.fit <- ergm(lazega.ergm ~ edges + gwesp(log(3), fixed=TRUE)
 + nodecov("Practice")
 + nodematch("Practice")
 + nodematch("Gender")
 + nodematch("Office"))


## -------------------------------------------------------------------
anova(lazega.ergm.fit)


## -------------------------------------------------------------------
summary(lazega.ergm.fit)


## -------------------------------------------------------------------
gof.lazega.ergm <- gof(lazega.ergm.fit)
par(mfrow=c(2, 2))
plot(gof.lazega.ergm)

