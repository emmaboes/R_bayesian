rm(list=ls())
setwd("~/Desktop")
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
head(nds$y_post)

plot(NULL,ylim=c(0,100),xlim=c(0,100))
#for (i in 60:70) {
#lines(1:100,cumsum(nds$y_post[i,]),col='#0000ee33',lwd=2)
#} 
lines(1:length(y),cumsum(y),lwd=2)
for (INDX in 1:dim(nds$y_post)[1]) {
  lines(1:100,cumsum(nds$y_post[INDX,]),col='#0000ee33',lwd=2)
}
#Grafica todos los renglones. Para hacer sollo con muestras:
dev.off()
plot(NULL,ylim=c(0,100),xlim=c(0,100))
for (INDX in sample(dim(nds$y_post)[1],40)) {
  lines(1:100,cumsum(nds$y_post[INDX,]),col='#0000ee33',lwd=2)
}
lines(1:length(y),cumsum(y),lwd=2)


