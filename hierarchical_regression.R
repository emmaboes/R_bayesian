rm(list=ls())
salex <- read.csv('~/Documents/Teaching/IAD_2_spring2019/Linear Regression/salary-experience.csv',
                  skip = 2)
head(salex)

x <- salex$experience
y <- salex$salary
n_obs <- nrow(salex)

write('model{

b0_post~dunif(4e+04,8e+04)
b0_prior~dunif(4e+04,8e+04)
b1_post~dunif(0,3000)
b1_prior~dunif(0,3000)
sigma_post~dunif(0,20000)
sigma_prior~dunif(0,20000)
lambda_post <- 1/sigma_post^2
lambda_prior <- 1/sigma_prior^2
for(i in 1:n_obs){
  mu_y_post[i] <- b0_post+b1_post*x[i]
  mu_y_prior[i] <- b0_prior+b1_prior*x[i]
  
  y[i]~dnorm(mu_y_post[i],lambda_post)
  y_post[i]~dnorm(mu_y_post[i],lambda_post)
  y_prior[i]~dnorm(mu_y_prior[i],lambda_prior)
}

}','salex.bug')

obs <- list('x','y','n_obs')
unobs <- c('b1_prior','b0_prior','sigma_prior',
           'b1_post','b0_post','sigma_post',
           'y_post')

library('R2jags')
inferencia<-jags( 
  data=obs,
  parameters.to.save=unobs, 
  model.file='salex.bug',
  n.chains=3,
  n.iter=1500, 
  n.burnin=1000,
  n.thin=1, 
  DIC=T)
nds <- inferencia$BUGSoutput$sims.list

plot(salex$experience,
     salex$salary,
     ylim=c(0,150000))
for(i in 1:n_obs){
  points(rep(x[i],dim(nds$y_post)[1]),
         nds$y_post[,i],
         col='#0000ee44')
}
points(x,y,pch=21,bg='#dddddd',lwd=2,cex=2)

hist(nds$b0_post)
hist(nds$b1_post)




# Regresión por cada departamento
rm(list=ls())
salex <- read.csv('~/Documents/Teaching/IAD_2_spring2019/Linear Regression/salary-experience.csv',
                  skip = 2)
x <- salex$experience
y <- salex$salary
d <- as.numeric(salex$department)
n_obs <- nrow(salex)

write('model{

for(dept in 1:5){
  b0_post[dept]~dunif(4e+04,8e+04)
  b0_prior[dept]~dunif(4e+04,8e+04)
  b1_post[dept]~dunif(0,3000)
  b1_prior[dept]~dunif(0,3000)
  sigma_post[dept]~dunif(0,20000)
  sigma_prior[dept]~dunif(0,20000)
  lambda_post[dept] <- 1/sigma_post[dept]^2
  lambda_prior[dept] <- 1/sigma_prior[dept]^2
}

for(i in 1:n_obs){
  
  mu_y_post[i] <- b0_post[d[i]]+b1_post[d[i]]*x[i]
  mu_y_prior[i] <- b0_prior[d[i]]+b1_prior[d[i]]*x[i]
  
  y[i]~dnorm(mu_y_post[i],lambda_post[d[i]])
  y_post[i]~dnorm(mu_y_post[i],lambda_post[d[i]])
  y_prior[i]~dnorm(mu_y_prior[i],lambda_prior[d[i]])
  
}


}','salex_dept.bug')

obs <- list('x','y','d','n_obs')
unobs <- c('b1_prior','b0_prior','sigma_prior',
           'b1_post','b0_post','sigma_post',
           'y_post')

library('R2jags')
inferencia<-jags( 
  data=obs,
  parameters.to.save=unobs, 
  model.file='salex_dept.bug',
  n.chains=3,
  n.iter=1500, 
  n.burnin=1000, 
  n.thin=1, 
  DIC=T)
nds <- inferencia$BUGSoutput$sims.list

plot(NULL,xlim=c(0,9),ylim=c(4e+04,10e+04))
for(k in 1:5){
  for(i in 1:30){
    abline(nds$b0_post[i,k],
           nds$b1_post[i,k],
           col=k)
  }
}

points(salex$experience,
       salex$salary,
       pch=21,cex=2,lwd=2,
       bg=salex$department)








# Regresión por cada departamento
rm(list=ls())
salex <- read.csv('~/Documents/Teaching/IAD_2_spring2019/Linear Regression/salary-experience.csv',
                  skip = 2)
x <- salex$experience
y <- salex$salary
d <- as.numeric(salex$department)
n_obs <- nrow(salex)

write('model{
mu_b0_post~dunif(5e+04,7e+04)
mu_b0_prior~dunif(5e+04,7e+04)
sigma_b0_post~dunif(0,20000)
sigma_b0_prior~dunif(0,20000)

mu_b1_post~dunif(500,2000)
mu_b1_prior~dunif(500,2000)
sigma_b1_post~dunif(0,2000)
sigma_b1_prior~dunif(0,2000)

for(dept in 1:5){
  # b0_post[dept]~dunif(4e+04,8e+04)
  # b0_prior[dept]~dunif(4e+04,8e+04)
  b0_post[dept]~dnorm(mu_b0_post,1/sigma_b0_post^2)
  b0_prior[dept]~dnorm(mu_b0_prior,1/sigma_b0_prior^2)
  # b1_post[dept]~dunif(0,3000)
  # b1_prior[dept]~dunif(0,3000)
  b1_post[dept]~dnorm(mu_b1_post,1/sigma_b1_post^2)
  b1_prior[dept]~dnorm(mu_b1_prior,1/sigma_b1_prior^2)
  sigma_post[dept]~dunif(0,20000)
  sigma_prior[dept]~dunif(0,20000)
  lambda_post[dept] <- 1/sigma_post[dept]^2
  lambda_prior[dept] <- 1/sigma_prior[dept]^2
}

for(i in 1:n_obs){
  
  mu_y_post[i] <- b0_post[d[i]]+b1_post[d[i]]*x[i]
  mu_y_prior[i] <- b0_prior[d[i]]+b1_prior[d[i]]*x[i]
  
  y[i]~dnorm(mu_y_post[i],lambda_post[d[i]])
  y_post[i]~dnorm(mu_y_post[i],lambda_post[d[i]])
  y_prior[i]~dnorm(mu_y_prior[i],lambda_prior[d[i]])
  
}


}','salex_dept_h.bug')

obs <- list('x','y','d','n_obs')
unobs <- c('b1_prior','b0_prior','sigma_prior',
           'b1_post','b0_post','sigma_post',
           'y_post',
           'mu_b0_post',
           'mu_b0_prior',
           'sigma_b0_post',
           'sigma_b0_prior')

library('R2jags')
inferencia<-jags( 
  data=obs,
  parameters.to.save=unobs, 
  model.file='salex_dept_h.bug',
  n.chains=3,
  n.iter=5500, 
  n.burnin=1000, 
  n.thin=1, 
  DIC=T)
nds <- inferencia$BUGSoutput$sims.list

plot(NULL,xlim=c(0,9),ylim=c(4e+04,10e+04))
for(k in 1:5){
  for(i in 1:30){
    abline(nds$b0_post[i,k],
           nds$b1_post[i,k],
           col=k)
  }
}

points(salex$experience,
       salex$salary,
       pch=21,cex=2,lwd=2,
       bg=salex$department)


hist(nds$mu_b0_prior,breaks=100,col='#ee003388',ylim=c(0,300))
hist(nds$mu_b0_post,breaks=100,col='#3300ee88',add=T)


for(d in 1:5){
hist(nds$b1_post[,d],breaks=100,col=d,
     ylim=c(0,1000),
     xlim=c(0,3000),
     add=d>1)
}
