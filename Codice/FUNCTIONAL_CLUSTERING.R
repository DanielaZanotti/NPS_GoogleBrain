#################### LIBRARIES ####################


library(roahd)
library(readr)
library(tidyverse) 
library(roahd)
library(fdakmapp)
library(data.table)

##################### FUNCTIONAL CLUSTERING ################

x = read.table("Data/time_step_row.csv",header=TRUE,sep=",")
y = read.table("Data/u_in_row.csv",header=TRUE,sep=",")

y<-as.matrix(u_in_row)
x<-as.matrix(time_step_row)

set.seed(2304)

#################### L2 CLUSTERING ########################


n_cluster=3


fdakma_noalign_l2 <- kmap(
  x=x, y=y, n_clust = n_cluster, 
  warping_method = 'noalign', 
  similarity_method = 'l2', 
  center_method = 'mean',
  fence=TRUE
)

kmap_show_results(fdakma_noalign_l2)

x11()
par(mfrow=c(3,n_cluster/3))
for (i in seq(1,n_cluster )){
  clus = y[which(fdakma_noalign_l2$labels == i),]
  time = x[which(fdakma_noalign_l2$labels == i),]
  matplot(t(time),t(clus), type='l', xlab='x', ylab='orig.func', col= "grey")
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
  matplot(t(time),t(clus), type='l', xlab='x', ylab='orig.func', col= "grey")
} 



