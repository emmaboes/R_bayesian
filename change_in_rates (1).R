rm(list=ls())
setwd("~/Documents/Teaching/IAD_2_spring2019")
volados <- read.csv('datos_binarios.csv')
names(volados)
head(volados)
tail(volados)

# Observadas
y <- volados$var2
n_obs <- length(y)
observadas <- list('y','n_obs')

# No observadas
no_observadas <- c('gamma_post',
                   'gamma_prior',
                   'y_prior',
                   'y_post')

# Modelo
write('
model{
  gamma_prior~dbeta(5,5)
  gamma_post~dbeta(5,5)

  for(k in 1:n_obs){
    y_prior[k]~dbinom(gamma_prior,1)
    y[k]~dbinom(gamma_post,1)
    y_post[k]~dbinom(gamma_post,1)
  }

}
','binarios.bug')

library('R2jags')
set.seed(12345)
inferencia<-jags( 
  data=observadas,
  parameters.to.save=no_observadas, 
  model.file='binarios.bug',
  n.chains=3,
  n.iter=500, 
  n.burnin=100, 
  n.thin=1, 
  DIC=T)
nds <- inferencia$BUGSoutput$sims.list
layout(1:2)
hist(nds$gamma_post,xlim=c(0,1),col='#00cc0066',border = NA)
hist(nds$gamma_prior,add=T,col='#ee000033',border = NA)
brks_y <- seq(-.1,1.1,.2)
hist(nds$y_post,xlim=c(0,1),col='#00cc0066',border = NA,freq = F,breaks=brks_y)
hist(nds$y_prior,add=T,col='#ee000033',border = NA,freq = F,breaks=brks_y)
hist(y,add=T,freq = F,breaks=brks_y)

dim(nds$gamma_post)

dev.off()
plot(NULL,ylim=c(0,100),xlim=c(0,100))
# for(INDX in 1:dim(nds$y_post)[1]){
# for(INDX in 60:70){
for(INDX in sample(dim(nds$y_post)[1],40)){
  lines(1:100,cumsum(nds$y_post[INDX,]),col='#0000ee13',lwd=2)
}
lines(1:length(y),cumsum(y),lwd=2)











rm(list=ls())
setwd("~/Documents/Teaching/IAD_2_spring2019")
volados <- read.csv('datos_binarios.csv')

# Observadas
y <- volados$var2
n_obs <- length(y)
ensayo <- 1:n_obs
observadas <- list('y','n_obs','ensayo')

# No observadas
no_observadas <- c('gamma',
                   'gamma_prior',
                   'pto_cambio',
                   'pto_cambio_prior',
                   'y_post',
                   'y_prior')
write('
model{
  # Rates of success
  gamma[1]~dbeta(5,5)
  gamma[2]~dbeta(5,5)
  gamma_prior[1]~dbeta(5,5)
  gamma_prior[2]~dbeta(5,5)

  # Trial of rate switch
  # dist_tau <- rep(1/n_obs,n_obs)
  dist_tau <- c(rep(0,65),rep(1/35,35))
  pto_cambio ~ dcat(dist_tau[])
  pto_cambio_prior ~ dcat(dist_tau[])

  for(k in 1:n_obs){
    z_prior[k] <- step(ensayo[k]-pto_cambio_prior)+1
    y_prior[k]~dbinom(gamma_prior[z[k]],1)
    z[k] <- step(ensayo[k]-pto_cambio)+1 # Nota: step(x)=0 si x<0; step(x)=1 si x>=0
    y[k]~dbinom(gamma[z[k]],1)
    y_post[k]~dbinom(gamma[z[k]],1)
  }
}
','change_detection.bug')

library('R2jags')
set.seed(12345)
inferencia<-jags( 
  data=observadas,
  parameters.to.save=no_observadas, 
  model.file='change_detection.bug',
  n.chains=3,
  n.iter=2000, 
  n.burnin=1000, 
  n.thin=1, 
  DIC=T)
nds <- inferencia$BUGSoutput$sims.list

col_prior <- '#006688'
col_post <- '#ee8800'
plot_layout <- cbind(rep(1,2),2:3,rep(4,2))
try(dev.off())
x11(width=11,height=6)
layout(plot_layout)
x_limits <- c(0,100)
hist(nds$pto_cambio_prior,breaks=seq(-.5,100.5,1),ylim=c(0,6000),xlim=x_limits,col=col_prior,border=NA)
hist(nds$pto_cambio,breaks=seq(-.5,100.5,1),xlim=x_limits,col=col_post,border=NA,add=T)
hist(nds$gamma_prior[,1],breaks=seq(0,1,0.02),ylim=c(0,2000),border=F,col=col_prior)
hist(nds$gamma[,1],breaks=seq(0,1,0.02),border=F,col=col_post,add=T)
hist(nds$gamma_prior[,2],breaks=seq(0,1,0.02),ylim=c(0,2000),border=F,col=col_prior)
hist(nds$gamma[,2],breaks=seq(0,1,0.02),border=F,col=col_post,add=T)
plot(NULL,ylim=c(0,100),xlim = x_limits)
for(i in sample(dim(nds$y_prior)[1],size=100)){
  lines(1:dim(nds$y_prior)[2],
        cumsum(nds$y_prior[i,]),
        col=col_prior)}
for(i in sample(dim(nds$y_prior)[1],size=100)){
  lines(1:dim(nds$y_post)[2],
        cumsum(nds$y_post[i,]),
        col=col_post)}
lines(1:length(y),cumsum(y),lwd=4)