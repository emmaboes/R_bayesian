rm(list=ls())
pi12 <- read.csv('~/pisa2012_subset_mat.csv')
head(pi12)

pi12 <- pi12[783:832,]


aciertos_personas <- apply(pi12,MARGIN=1,FUN=sum)
aciertos_reactivos <- apply(pi12,MARGIN=2,FUN=sum)

pi12 <- pi12[order(aciertos_personas,decreasing = T),]
pi12 <- pi12[,order(aciertos_reactivos,decreasing = T)]

n_pers <- length(aciertos_personas)
n_reac <- length(aciertos_reactivos)

try(dev.off())
quartz(width=3.75,height=7)
par(bg='#aaaaaa',mar=c(2,1.5,1,1))
plot(NULL,ylim=c(-n_pers,3),xlim=c(-1,n_reac+1),
     axes=F,ann=F)
mtext('reactivos',1,line=.5,cex=1.5,col='#555555')
mtext('personas',2,line=0,cex=1.5,col='#555555')
for(pp in 1:n_pers){
  text(-.5,-pp,pp,cex=.5,col='blue')
  text(n_reac+1,-pp,sum(pi12[pp,]),cex=.6,col='red',font=2)
  for(rr in 1:n_reac){
    color <- 'white'
    if(pi12[pp,rr]==1){
      color <- 'black'
    }
    points(rr,-pp,pch=21,bg=color,cex=1.2)
    if(pp==1){
      text(rr,0,names(pi12)[rr],srt=90,cex=.6,col='blue',adj=c(0,.5))
      text(rr,-n_pers-1.25,sum(pi12[,names(pi12)[rr]]),cex=.6,col='red',font=2,srt=90)
    }
  }
}

#theta es la variable observada que tenemos de los datos
# "P(Xpr = 1)" es una variable continuta.
#theta es una variable que pertenece al conjunto de personas en la prueba de Pisa.
#Beta, es una variable que pertenece al conjunto de reactivos respondido 
#Cuando el modelo encuentra un acierto, nos dir?? que la Beta del reactivo es menor que el theta de la persona.
#Cuando el modelo encuentra un fallo, nos dir?? que la Beta del reactivo es mayor que el theta de la persona.

#Theta es una distribuci??n normal ( "dnorm(0,1)" )
#Beta es una distribuci??n normal ( "dnorm()0,1" )

write('
      model{
      for(p in 1:n_pers){
      theta_prior[p]~dnorm(0,1)
      theta_post[p]~dnorm(0,1)
      }
      
      for(r in 1:n_reac){
      beta_prior[r]~dnorm(0,1)
      beta_post[r]~dnorm(0,1)
      gamma_prior[r]~dbeta(5,5)
      gamma_post[r]~dbeta(5,5)
      }
      #beta[12]~dnorm(0,1) -> se supone que con esto estamos declarando que el reactivo 12
      #tiene una dificultad igual a la media. 
      #beta[19]~dnorm(3,1) -> esta es una pregunta, en teor??a, m??s dif??cil como lo hemos declarado.
      #
      
      for(p in 1:n_pers){
      for(r in 1:n_reac){
      prob_prior[p,r] <- gamma_prior[r]+(1-gamma_prior[r])*(1/(1+exp(-(theta_prior[p]-beta_prior[r]))))
      prob_post[p,r] <- gamma_post[r]+(1-gamma_post[r])*(1/(1+exp(-(theta_post[p]-beta_post[r]))))
      pi12_prior[p,r]~dbinom(prob_prior[p,r],1)
      pi12[p,r]~dbinom(prob_post[p,r],1)
      pi12_post[p,r]~dbinom(prob_post[p,r],1)
      }
      }
      }
      ','lplm.bug')

observadas <- list('pi12','n_reac','n_pers')
no_observadas <- c('beta_post','theta_post','prob_post','pi12_post',
                   'gamma_prior','beta_prior',
                   'theta_prior','prob_prior','pi12_prior','gamma_post')

library('R2jags')
set.seed(123)
inferencia<-jags( 
  data=observadas,
  parameters.to.save=no_observadas, 
  model.file='lplm.bug',
  n.chains=3,
  n.iter=1000, 
  n.burnin=500, 
  n.thin=1, 
  DIC=T)

nds <- inferencia$BUGSoutput$sims.list

head(nds$beta) #Para poder saber el n??mero de personas y de reactivos
dim(nds$beta)

summary(nds$theta)
summary(as.vector(nds$theta))
#nos ayuda a ver los limites del histograma que vamos a hacer.
#para que, a la hora de hacer el plot, podamos comprender mejor los limites a poner

try(dev.off())
x11(width = 6,height = 4)
plot(NULL,xlim=c(-5,5),ylim = c(-400,400))
#persona <- 1
for(persona in 1:n_pers){
  hist(nds$theta_prior[,persona], breaks = seq(-5,5,0.2), plot = F) -> ht
  lines(ht$mids,ht$counts,col = "green")
  hist(nds$theta_post[,persona], breaks = seq(-5,5,0.2), plot = F) -> ht
  lines(ht$mids,ht$counts,col = "orange")
}

for(reactivo in 1:n_reac){
  hist(nds$beta_prior[,reactivo], breaks = seq(-6,6,0.2), plot = F) -> ht
  lines(ht$mids,-ht$counts,col = 'blue')
  hist(nds$beta_post[,reactivo], breaks = seq(-6,6,0.2), plot = F) -> ht
  lines(ht$mids,-ht$counts,col = 'red')
}

#Tarea
try(dev.off())
#quartz(width=3.75,height=7)
x11(width = 3.75,height = 7)
matriz<-pi12
for (i in 1:n_pers){
  for (j in 1:n_reac){
    matriz[i,j]<-median(nds$pi12_post[,i,j])
  }}
par(bg='#aaaaaa',mar=c(2,1.5,1,1))
plot(NULL,ylim=c(-n_pers,3),xlim=c(-1,n_reac+1),
     axes=F,ann=F)
mtext('reactivos',1,line=.5,cex=1.5,col='#555555')
mtext('personas',2,line=0,cex=1.5,col='#555555')
for(pp in 1:n_pers){
  text(-.5,-pp,pp,cex=.5,col='blue')
  text(n_reac+1,-pp,sum(matriz[pp,]),cex=.6,col='red',font=2)
  for(rr in 1:n_reac){
    color <- 'white'
    #if(pi12[pp,rr]==1){
    #if(nds$pi12_post[355,,][pp,rr]==1){
    if(median(nds$pi12_post[,pp,rr])==1){
      color <- 'black'
    }
    points(rr,-pp,pch=21,bg=color,cex=1.2)
    if(pp==1){
      text(rr,0,names(pi12)[rr],srt=90,cex=.6,col='blue',adj=c(0,.5))
      text(rr,-n_pers-1.25,sum(matriz[,names(matriz)[rr]]),cex=.6,col='red',font=2,srt=90)
    }
  }
}
