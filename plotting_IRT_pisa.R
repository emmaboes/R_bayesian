rm(list=ls())
# Data Reading
pi12 <- read.csv('~/Documents/Teaching/IAD_2_spring2019/pisa12/pisa2012_subset_mat.csv')
pi12 <- pi12[783:832,]
aciertos_personas <- apply(pi12,MARGIN=1,FUN=sum)
aciertos_reactivos <- apply(pi12,MARGIN=2,FUN=sum)
pi12 <- pi12[order(aciertos_personas,decreasing = T),]
pi12 <- pi12[,order(aciertos_reactivos,decreasing = T)]
n_pers <- length(aciertos_personas)
n_reac <- length(aciertos_reactivos)

# Bayesian Results
load('~/Documents/Teaching/IAD_2_spring2019/pisa12/jags_1plm_pisa.RData')
load('~/Documents/Teaching/IAD_2_spring2019/pisa12/jags_2plm_pisa.RData')
load('~/Documents/Teaching/IAD_2_spring2019/pisa12/jags_3plm_pisa.RData')

# nds <- nds_2plm

# Plotting Functions
matrix_01_1plm <- array(dim=c(n_pers,n_reac))
matrix_01_2plm <- array(dim=c(n_pers,n_reac))
matrix_01_3plm <- array(dim=c(n_pers,n_reac))
for(pp in  1:n_pers){
  for(rr in 1:n_reac){
    matrix_01_1plm[pp,rr] <- median(nds_1plm$pi12_post[,pp,rr])
    matrix_01_2plm[pp,rr] <- median(nds_2plm$pi12_post[,pp,rr])
    matrix_01_3plm[pp,rr] <- median(nds_3plm$pi12_post[,pp,rr])
  }
}

plot_datos <- function(matrix_01,title=NULL,ann=T){
  n_pers <- nrow(matrix_01)
  n_reac <- ncol(matrix_01)
  # try(dev.off())
  # x11(width=3.75,height=7)
  # pdf(file='pi12.pdf',width=3.75,height=7)
  # matrix_01
  par(bg='#aaaaaa',mar=c(2,1.5,1,1))
  plot(NULL,ylim=c(-n_pers,5),xlim=c(-1,n_reac+1),
       axes=F,ann=F)
  mtext(title,3,line=-1.5,cex=1.5,col='#555555')
  if(ann){
    mtext('reactivos',1,line=.5,cex=1.5,col='#555555')
    mtext('personas',2,line=0,cex=1.5,col='#555555')
  }
  for(pp in 1:n_pers){
    text(-.5,-pp,pp,cex=.5,col='blue')
    # text(n_reac+1,-pp,sum(pi12[pp,]),cex=.6,col='red',font=2)
    text(n_reac+1,-pp,sum(matrix_01[pp,]),cex=.6,col='red',font=2)
    for(rr in 1:n_reac){
      color <- 'white'
      # if(median(nds$pi12_post[,pp,rr])==1){
      if(matrix_01[pp,rr]==1){
        color <- 'black'
      }
      points(rr,-pp,pch=21,bg=color,cex=1.2)
      if(pp==1){
        text(rr,0,names(pi12)[rr],srt=90,cex=.6,col='blue',adj=c(0,.5))
        # text(rr,-n_pers-1.25,sum(matrix_01[,names(pi12)[rr]]),cex=.6,col='red',font=2,srt=90)
        text(rr,-n_pers-1.25,sum(matrix_01[,rr]),cex=.6,col='red',font=2,srt=90)
      }
    }
  }
  # dev.off()
}


pdf(file='~/Documents/Teaching/IAD_2_spring2019/pisa12/post_pred.pdf',width=10,height=6)
layout(matrix(1:4,ncol=4))
plot_datos(pi12,title='data',ann=F)
plot_datos(matrix_01_1plm,title='1plm',ann=F)
plot_datos(matrix_01_2plm,title='2plm',ann=F)
plot_datos(matrix_01_3plm,title='3plm',ann=F)
mtext('reactivos',1,line=-1.5,cex=1.5,col='#555555',outer=T)
mtext('personas',2,line=-1.75,cex=1.5,col='#555555',outer=T)
dev.off()




model <- nds_1plm
n_lines <- 30
indx <- sample(dim(model$alpha_post)[1],n_lines)

# ICC
pdf(file='~/Documents/Teaching/IAD_2_spring2019/pisa12/ICC.pdf',width = 12,height = 6)
# x11(width=120,height=80)
layout(matrix(1:24,ncol=6))
par(cex=1.5)
for(item in 1:22){
  # item <- 22
  par(mar=rep(1,4),mgp=c(3,0.25,0),tck=-0.04,cex.axis=.5)
  plot(NULL,xlim=c(-5,5),ylim=c(0,1),axes=F)
  axis(1,at=c(-5,0,5),padj=-1.5)
  axis(2,at=c(0,0.5,1),las=1)
  abline(v=0,lty='dashed')
  mtext(names(pi12)[item],3,font=2) # Make sure items (and people) have been reordered after reading the .csv
  for(i in indx){
    a <- model$alpha_post[i,item]
    b <- model$beta_post[i,item]
    g <- model$gamma_post[i,item]
    color <- '#00338888'
    th <- seq(-5,5,length.out = 100)
    icc <- g+(1-g)*(1/(1+exp(a*(th-b))))
    p_th <- icc
    q_th <- 1-icc
    iic <- a^2*(q_th/p_th)*((p_th-g)/(1-g))^2
    lines(th,icc,col=color)
    lines(th,iic,col='blue')
  }
}
dev.off()


# item <- 22
par(mar=rep(3,4),mgp=c(3,0.25,0),tck=-0.04,cex.axis=1)
plot(NULL,xlim=c(-5,5),ylim=c(0,130),axes=F)
axis(1,at=c(-5,0,5),padj=1)
axis(2,at=c(0,100),las=1,hadj=2)
# abline(v=0,lty='dashed')
# mtext(names(pi12)[item],3,font=2) # Make sure items (and people) have been reordered after reading the .csv
th <- seq(-5,5,length.out = 100)
iic <- vector(mode='numeric',length = length(th))
for(i in indx){
  for(item in 1:22){
    a <- model$alpha_post[i,item]
    b <- model$beta_post[i,item]
    g <- model$gamma_post[i,item]
    color <- '#00338888'
    icc <- g+(1-g)*(1/(1+exp(a*(th-b))))
    p_th <- icc
    q_th <- 1-icc
    iic <- iic+a^2*(q_th/p_th)*((p_th-g)/(1-g))^2
    # lines(th,icc,col=color)
  }
  lines(th,iic,col='#ee8800')
}


