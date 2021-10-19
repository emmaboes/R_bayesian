rm(list=ls())
library('R2jags')

write('
      model{
      # Prior Distribution for Rate Theta
      theta_prior ~ dbeta(3,5) # En general, es necesario duplicar el modelo para rastrear/examinar los supuestos a-priori
      theta_post ~ dbeta(3,5)

      # Observed Counts
      k_prior ~ dbin(theta_prior,25) # Expectativa inicial sobre observables
      k_obs ~ dbin(theta_post,25) # Inferencia sobre no observables dadas las observaciones
      k_post ~ dbin(theta_post,25) # Expectativa final sobre observables 
      


      # Nodo sin relación con el modelo, para rastrear nodos 'fantasma':
      nada~dnorm(15,25)
      }
      ','modelo.bug')

parametros <- c('theta_prior','k_prior',
                'theta_post','k_post')

k_obs <- 13
nada <- 25 # 'observación' sin relación con el modelo
datos_jags <- list('k_obs','nada')

inferencia<-jags( 
  data=datos_jags,
  parameters.to.save=parametros, 
  model.file='modelo.bug',
  n.chains=3,
  n.iter=50000, 
  n.burnin=1000, 
  n.thin=4, 
  DIC=T)

# traceplot(inferencia)

nodos <- inferencia$BUGSoutput$sims.list

x11(width = 6,height = 6) # Especifica las dimensiones del siguiente 'plot device'
layout(matrix(1:4,ncol=2)) # Prepara el siguiente 'plot device' para que incluya cuatro gráficas
par(
  mar=c(4,4,1,1), # Especifica los márgenes de cada gráfica dentro del 'plot device' en uso (empezando por el inferior, en dirección de las manecillas)
  mgp=c(1.5,.5,0) # Especifica las distancias de 1) el texto de los ejes al eje (1.5), 2) la distancia entre los valores del eje al eje (0.5) y 3) la posición de los ejes
    ) 
hist(nodos$theta_prior,breaks = 100,col='black',xlim=c(0,1))
hist(nodos$k_prior,breaks = 100,col='black',xlim=c(0,25))
hist(nodos$theta_post,breaks = 100,col='black',xlim=c(0,1))
hist(nodos$k_post,breaks = 100,col='black',xlim=c(0,25))
points(k_obs,0,pch=21,cex=2,bg='#99334499') # Agrega un punto en la observación de k, para compararla contra la expectativa posterior del modelo