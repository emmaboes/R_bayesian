rm(list=ls())
pi12 <- read.csv('~/Documents/Teaching/IAD_2_spring2019/pisa2012_subset_mat.csv')
pi12 <- pi12[783:832,]

aciertos_personas <- apply(pi12,MARGIN=1,FUN=sum)
aciertos_reactivos <- apply(pi12,MARGIN=2,FUN=sum)

pi12 <- pi12[order(aciertos_personas,decreasing = T),]
pi12 <- pi12[,order(aciertos_reactivos,decreasing = T)]

n_pers <- length(aciertos_personas)
n_reac <- length(aciertos_reactivos)

try(dev.off())
x11(width=3.75,height=7)
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
