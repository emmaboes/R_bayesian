rm(list=ls())
setwd("~/Desktop")
volados <- read.csv('datos_binarios.csv')
#names(volados)
#head(volados)
#tail(volados)

# Observadas
y <- volados$var2
n_obs <- length(y)
ensayo<-1:n_obs
observadas <- list('y','y_post','n_obs', 'ensayo')
# No observadas
no_observadas <- c('gamma', 'gamma_post',
                   'pto_cambio')

# Modelo
write('
      model{
  #Rates of success
      gamma_prior[1]~dbeta(5,5)
      gamma_post[1]~dbeta(5,5)
      gamma_prior[2]~dbeta(5,5)
      gamma_post[2]~dbeta(5,5)
      # Trial of rate switch
      pto_cambio ~ dcat(dist_tau[])
      dist_tau <- rep(1/n_obs,n_obs)
      for(k in 1:n_obs){
      z[k] <- step(ensayo[k]-pto_cambio) +1 # Nota: ste(x)=0 si x < 0; step(x)=1 si x ≥ 0
      y[k]~dbinom(gamma_post[z[k]],1)
      y_post[k]~dbinom(gamma_post[z[k]],1)}
      
      }
      ','change_direction.bug')
#tarea: regresar un registro acumulativo de todas las lineas de y_post 
#Mandar antes del viernes. Extra_: mostrar el modelo a priori sobre los datos y el punto de cambio
library('R2jags')
set.seed(12345)
inferencia<-jags( 
  data=observadas,
  parameters.to.save=no_observadas, 
  model.file='change_direction.bug',
  n.chains=3,
  n.iter=500, 
  n.burnin=100, 
  n.thin=1, 
  DIC=T)
nds <- inferencia$BUGSoutput$sims.list

layout(matrix(1:2,ncol=2))
hist(nds$pto_cambio,xlim=c(0,100),col='#00cc0066',border = NA)
plot(1:length(y),cumsum(y),type ='l', xlim=c(50,90))
hist(nds$gamma_post,col='#ee000033',border = NA)
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
