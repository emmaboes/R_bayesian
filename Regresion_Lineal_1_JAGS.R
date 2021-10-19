rm(list=ls())
setwd("/Volumes/EYBE/Investigación y Analisis de Datos/14 de mayo/Linear Regression")
lr <- read.csv('Jan2015_RegresionLineal_1.csv')

try(dev.off())
plot(lr$x,lr$y, xlim=c(0,40), ylim=c(30,80))

plot(NULL, xlim = c(-5,5), ylim = c(-5,5))
abline(h=0,lty='dashed');abline(v=0,lty='dashed')
x <- seq(-5,5,.01)
y1 <- (+1.5)*x+(-3)
y2 <- (-0.333)*x+(+2)
lines(x,y1, col = 'blue', lwd = 2)
lines(x,y2, col = 'red', lwd = 2)
# y = f(x) = BiX + B0
#  = m*x + b

# B0 es el lugar donde corta la linea en el eje "y"
# tan(alpha) = A/B
#La linea relevante, por ejemplo cuando la recta es decreciente, será aquella con la que se forma
# una linea paralela a el eje de las equis "x"

# "Por cada unidad que se gana en equis, se gana (si es positivo) o se pierde (si es negativo)
# en el eje "y"


x<- lr$x
y<-lr$y
n_casos <- length(y)

x_pred <- c(12,30)  #no observados de x cuya predicciòn en y es relevante 
n_casos_pred <- length(x_pred)

write('
      model{
      
      #Priors
      b0_prior ~ dnorm (0,1)
      b1_prior ~ dnorm (0,0.1) T(0,)
      sigma_prior ~ dnorm (0,1) T(0,)
      lambda_prior<-1/sigma_prior^2
      #Post
      b0_post ~ dnorm (0,1)
      b1_post ~ dnorm (0,0.1) T(0,)
      sigma_post ~ dnorm (0,1) T(0,)
      lambda_post<-1/sigma_post^2
      
      for (i in 1: n_casos){
      mu_y_prior[i]<- b0_prior+b1_prior*x[i]
      mu_y_post[i]<- b0_post+b1_post*x[i]
      y_prior[i] ~ dnorm (mu_y_prior[i],lambda_prior)
      y[i] ~ dnorm (mu_y_post[i],lambda_post)
      y_post[i] ~ dnorm (mu_y_post[i],lambda_post)      
      }
      
      for(k in 1:n_casos_pred){
      mu_y_pred_post[k] <- b0_post+b1_post*x_pred[k]
      y_pred_post[k] ~ dnorm(mu_y_pred_post[k],lambda_post)
      }
      
      
      }
      ','reglin.bug')

observadas<-list('y', 'n_casos','x','x_pred','n_casos_pred')
no_observadas<-c('b0_prior','b0_post','b1_prior','b1_post',
                 'sigma_prior','sigma_post','lambda_prior',
                 'lambda_post','mu_y_prior','mu_y_post','y_prior','y_post',
                 'y_pred_post','mu_y_pred_post')

library('R2jags')
set.seed(123)
inferencia<-jags( 
  data=observadas,
  parameters.to.save=no_observadas, 
  model.file='reglin.bug',
  n.chains=3,
  n.iter=1500, 
  n.burnin=1000, 
  n.thin=1, 
  DIC=T)

nds <- inferencia$BUGSoutput$sims.list

plot(lr$x,lr$y, xlim = c(10,30), ylim = c(30,70))

# which(x==min(x)) es lo mismo que la función que tenemos como which.min
try(dev.off())


obs <- 69
points(
  rep(x[obs],dim(nds$y_post)[1]),
  #función "rep" repite el primer argumento tantas veces cómo se le indique, con las segunda parte.
  # rep(argumento,dimensión)
  nds$y_post[,obs],
  col= 'red'
)
points(x[obs],y[obs],pch=16)



# Máximo

which.max(x)

obs2 <- 80 
points(
  rep(x[obs2],dim(nds$y_post)[1]),
  #función "rep" repite el primer argumento tantas veces cómo se le indique, con las segunda parte.
  # rep(argumento,dimensión)
  nds$y_post[,obs2],
  col= 'red'
)
points(x[obs2],y[obs2],pch=16)

# Minimo

which.min(x)

obs <- 69
points(
  rep(x[obs],dim(nds$y_post)[1]),
  #función "rep" repite el primer argumento tantas veces cómo se le indique, con las segunda parte.
  # rep(argumento,dimensión)
  nds$y_post[,obs],
  col= 'red'
)
points(x[obs],y[obs],pch=16)

try(dev.off())
quartz()

plot(lr$x,lr$y, xlim = c(0,40), ylim = c(0,100))
for(j in 1:n_casos){
  points(
    rep(x[j],dim(nds$y_post)[1]),
    #función "rep" repite el primer argumento tantas veces cómo se le indique, con las segunda parte.
    # rep(argumento,dimensión)
    nds$y_post[,j],
    col= 'red'
  )
}
points(x,y,pch=16) 


for(pred in 1:length(x_pred)){
  points(
    rep(x_pred[pred],dim(nds$y_pred_post)[1]),
    #función "rep" repite el primer argumento tantas veces cómo se le indique, con las segunda parte.
    # rep(argumento,dimensión)
    nds$y_pred_post[,pred],
    col= 'blue'
  )
}


#Tarea: Histograma de prior y posterior sobre pendiente e intercepto.

try(dev.off())
x11(width = 20,height = 10)
layout(matrix(1:2,ncol=2)) 

plot(NULL,xlim=c(-5,5),ylim = c(0,500), main= "B0", xlab="B0", ylab="Frequency")
hist(nds$b0_prior, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5),add = T)
hist(nds$b0_post, col = rgb(red = 0, green = 0, blue =  1, alpha = 0.5), add=T)
plot(NULL, xlim=c(-1,13),ylim = c(0,500), main= "B1", xlab="B1", ylab="Frequency")
hist(nds$b1_prior, xlim=c(-1,13), ylim = c(0,700), col = rgb(red = 1, green = 0, blue = 0, alpha = 0.5),add=T)
hist(nds$b1_post, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.5), add=T)

