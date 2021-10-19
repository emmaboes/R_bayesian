# Infiriendo una tasa de éxito con varias fuentes de información
# Lee, MD and Wagenmakers, E-J. Bayesian Cognitive Modeling, a practical course
# p. 43 - 44

rm(list=ls())

write('
      model{

      k1_prior ~ dbin(theta_prior,n1)
      k2_prior ~ dbin(theta_prior,n2)
      theta_prior ~ dbeta(1,1)

      k1_obs ~ dbin(theta_post,n1)
      k2_obs ~ dbin(theta_post,n2)
      theta_post ~ dbeta(1,1)

      k1_post ~ dbin(theta_post,n1)
      k2_post ~ dbin(theta_post,n2)

      # nada~dnorm(15,25) # Para ver priors!!

      }','cruzazul.bug')

no_observadas <- c('k1_prior','k2_prior',
                   'k1_post','k2_post',
                   'theta_prior',
                   'theta_post')
observadas <- list('n1','n2','k1_obs','k2_obs')

n1 <- 10
n2 <- 15
k1_obs <- 1
k2_obs <- 13

library('R2jags')
inferencia<-jags( 
  data=observadas,
  parameters.to.save=no_observadas, 
  model.file='cruzazul.bug',
  n.chains=3,
  n.iter=50000, 
  n.burnin=100, 
  n.thin=1, 
  DIC=T)

nds <- inferencia$BUGSoutput$sims.list

col_prior <- '#00881144'
col_post <- '#88002244'
try(dev.off())
x11(width = 5,height =5)
layout(matrix(1:4,ncol=2))
hist(nds$theta_post,col=col_post)
hist(nds$theta_prior,col=col_prior,add=T)
hist(nds$k1_post,breaks=seq(-.5,10.5,1),col=col_post)
hist(nds$k1_prior,breaks=seq(-.5,10.5,1),add=T,col=col_prior)
points(k1_obs,0,pch=16,cex=4,col='red')
hist(nds$k2_post,breaks=seq(-.5,15.5,1),col=col_post)
hist(nds$k2_prior,breaks=seq(-.5,15.5,1),add=T,col=col_prior)
points(k2_obs,0,pch=16,cex=4,col='red')




# Mismo modelo, expresado en notación de platos (fig. 3.6 en el libro):

rm(list=ls())

write('
model{
  for(i in 1:tot_obs){
    k_prior[i] ~ dbin(theta_prior,n[i])
  }
  theta_prior ~ dbeta(1,1)
  
  for(i in 1:tot_obs){
    k_obs[i] ~ dbin(theta_post,n[i])
  }

  theta_post ~ dbeta(1,1)

  for(i in 1:tot_obs){
    k_post[i] ~ dbin(theta_post,n[i])
  }
  
}','platos.bug')

no_observadas <- c('k_prior',
                   'k_post',
                   'theta_prior',
                   'theta_post')
observadas <- list('n','k_obs','tot_obs')

n <- c(10,15,20,25,12,35,50)
k_obs <- c(3,6,6,8,3,1,18)
tot_obs <- length(k_obs)

library('R2jags')
inferencia<-jags( 
  data=observadas,
  parameters.to.save=no_observadas, 
  model.file='platos.bug',
  n.chains=3,
  n.iter=5000, 
  n.burnin=100, 
  n.thin=1, 
  DIC=T)

nds <- inferencia$BUGSoutput$sims.list

col_prior <- '#00881144'
col_post <- '#88002244'
try(dev.off())
x11(width = 5,height =5)
layout(matrix(1:9,ncol=3))
hist(nds$theta_post,col=col_post)
hist(nds$theta_prior,col=col_prior,add=T)
# hist(nds$k_post[,1],breaks=seq(-.5,10.5,1),col=col_post)
# hist(nds$k_prior[,1],breaks=seq(-.5,10.5,1),add=T,col=col_prior)
# points(k_obs[1],0,pch=16,cex=4,col='red')
# hist(nds$k_post[,2],breaks=seq(-.5,15.5,1),col=col_post)
# hist(nds$k_prior[,2],breaks=seq(-.5,15.5,1),add=T,col=col_prior)
# points(k_obs[2],0,pch=16,cex=4,col='red')
for(g in 1:tot_obs){
  hist(nds$k_post[,g],breaks=seq(-.5,n[g]+.5,1),col=col_post)
  hist(nds$k_prior[,g],breaks=seq(-.5,n[g]+.5,1),add=T,col=col_prior)
  points(k_obs[g],0,pch=16,cex=4,col='red')
}

# Fin del código