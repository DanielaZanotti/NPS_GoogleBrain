#################### LIBRARIES ####################


library(roahd)
library(readr)
library(tidyverse) 
library(roahd)
library(fdakmapp)
library(data.table)
library(fda)

##################### FUNCTIONAL CLUSTERING ################

x = read.table("Data/time_step_row.csv",header=TRUE,sep=",")
y = read.table("Data/u_in_row.csv",header=TRUE,sep=",")

y<-as.matrix(u_in_row)
x<-as.matrix(time_step_row)

set.seed(2304)

##################### SMOOTHING ################

abscissa <- seq(0, 1, length.out=30)
NT <- length(abscissa)
ty <- t(y)

m <- 6        # spline order 
degree <- m-1    # spline degree 

# generalized cross-validation
nbasis <- 7:30
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.bspline.basis(c(0,1), nbasis[i], m)  #c(0,intervallotemporale)
  gcv[i] <- smooth.basis(abscissa, ty, basis)$gcv
}
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]

nbasis <- nbasis[which.min(gcv)]

breaks <- abscissa#[((0:15)*2)+1]

basis <- create.bspline.basis(breaks, norder=m)
functionalPar <- fdPar(fdobj=basis, Lfdobj=3, lambda=1e-8) 

Xss <- smooth.basis(abscissa, ty, functionalPar)

Xss0 <- eval.fd(abscissa, Xss$fd, Lfd=0)
Xss1 <- eval.fd(abscissa, Xss$fd, Lfd=1)

abs <- c()
for( i in 1:30)
{
  abs <- rbind(abs, abscissa)
}

par(mfrow=c(2,1))
plot(t(x[3,]),t(y[3,]),xlab="t",ylab="observed data", type='l', ylim = c(-50, 50))
points(t(x[3,]),t(Xss0)[3,] ,type="l",col="blue",lwd=2)
points(t(x[3,]),t(Xss1)[3,] ,type="l",col="red",lwd=2)

y_smooth <- t(Xss0)
y1 <- t(Xss1)

#################### L2 CLUSTERING ########################


n_cluster=25


fdakma_noalign_l2 <- kmap(
  x=x, y=y, n_clust = n_cluster, 
  warping_method = 'noalign', 
  similarity_method = 'l2', 
  center_method = 'mean',
  fence=TRUE
)

kmap_show_results(fdakma_noalign_l2)

x11()
par(mfrow=c(5,3))
for (i in seq(1,n_cluster )){
  clus = y[which(fdakma_noalign_l2$labels == i),]
  time = x[which(fdakma_noalign_l2$labels == i),]
  matplot(t(time),t(clus), type='l', xlab='x', ylab='orig.func', col= "blue")
} 


############################# SILHOUETTE #####################################

#Silhoutte Analysis


n_cluster=50
wss = c()
for(i in seq(1,n_cluster) ){
  print(i)
  m = c()
  its = 0
  while(its <= 5){
    fdakma_noalign_pearson <- kmap(
      x=x, y=y, n_clust = i, 
      warping_method = 'noalign', 
      similarity_method = 'pearson', 
      center_method = 'mean',
      fence=TRUE
    )
    its = its + 1
    wss.curr = sum( (1 - abs(fdakma_noalign_pearson$similarity.final))^2 )
    m = c(m,wss.curr)
  }
  wss = c(wss,mean(m))
}

plot(wss , type = "l")




############################ PEARSON CLUSTERING #################################


n_cluster=25

fdakma_pearson <- kmap(
  x=x, y=y, n_clust = n_cluster, 
  warping_method= 'noalign',
  similarity_method = 'pearson', 
  center_method = 'mean',
  fence=TRUE
)

kmap_show_results(fdakma_pearson)

x11()
par(mfrow=c(5,5))
for (i in seq(1,n_cluster )){
  clus = y[which(fdakma_pearson$labels == i),]
  time = x[which(fdakma_pearson$labels == i),]
  matplot(t(time),t(clus), type='l', xlab='x', ylab='orig.func', col= "blue")
} 



