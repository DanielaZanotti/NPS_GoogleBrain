library(fdakmapp)

tt <- rbind(train_until1, test_until1)

#time
time = data.frame()
for(i in unique(tt$breath_id)  ){
  x1=tt[which(tt$breath_id==i),'time_step']
  time = rbind(time, t(x1))
}

#u_in
uin = data.frame()
for(i in unique(tt$breath_id)  ){
  x1=tt[which(tt$breath_id==i),'u_in']
  uin = rbind(uin, t(x1))
}

#pressure
pres = data.frame()
for(i in unique(tt$breath_id) ){
  x4=tt[which(tt$breath_id==i), 'pressure']
  pres = rbind(pres, t(x4))
}

times = as.matrix(time)
uins = as.matrix(uin)
press = as.matrix(pres)

# Plot of original functions
matplot(t(times), t(uins), type='l', xlab='time step', ylab='u in')
title ('Original functions')

x <- c()
y <- c()
for (i in unique(tt$breath_id))
{
  t <- tt[which(tt$breath_id==i), 'time_step']
  u <- tt[which(tt$breath_id==i), 'u_in']
  x <- rbind(x, t)
  y <- rbind(y, u)
}



set.seed(2304)

n_cluster=3

fdakma_shift <- kmap(
  x=x, y=y, n_clust = n_cluster, 
  warping_method= 'shift',
  similarity_method = 'l2', 
  center_method = 'mean',
  fence=TRUE
)

kmap_show_results(fdakma_shift)

x11()
par(mfrow=c(3,n_cluster/3))
for (i in seq(1,n_cluster )){
  clus = y[which(fdakma_shift$labels == i),]
  time = x[which(fdakma_shift$labels == i),]
  matplot(t(time),t(clus), type='l', xlab='x', ylab='orig.func', col= "grey")
} 
matplot(t(time),t(uins), type='l', xlab='x', ylab='orig.func', col= fdakma_shift$labels)
matplot(t(time),t(press), type='l', xlab='x', ylab='orig.func', col= fdakma_shift$labels)


fdakma_affine <- kmap(
  x=x, y=y, n_clust = n_cluster, 
  warping_method = 'affine', 
  similarity_method = 'l2', 
  center_method = 'mean'
)

kmap_show_results(fdakma_affine)

x11()
par(mfrow=c(3,n_cluster/3))
for (i in seq(1,n_cluster )){
  clus = y[which(fdakma_affine$labels == i),]
  time = x[which(fdakma_affine$labels == i),]
  matplot(t(time),t(clus), type='l', xlab='x', ylab='orig.func', col= "grey")
} 
matplot(t(time),t(uins), type='l', xlab='x', ylab='orig.func', col= fdakma_affine$labels)
matplot(t(time),t(press), type='l', xlab='x', ylab='orig.func', col= fdakma_affine$labels)


fdakma_noalign <- kmap(
  x=x, y=y, n_clust = n_cluster, 
  warping_method = 'noalign', 
  similarity_method = 'l2', 
  center_method = 'mean',
  fence=TRUE
)

kmap_show_results(fdakma_noalign)

x11()
par(mfrow=c(n_cluster,1))
for (i in seq(1,n_cluster )){
  clus = y[which(fdakma_noalign$labels == i),]
  time = x[which(fdakma_noalign$labels == i),]
  matplot(t(time),t(clus), type='l', xlab='x', ylab='orig.func', col = 'blue')
} 
x11()
matplot(t(time),t(uins), type='l', xlab='x', ylab='orig.func', col= fdakma_noalign$labels)
x11()
matplot(t(time),t(press), type='l', xlab='x', ylab='orig.func', col= fdakma_noalign$labels)

fdakma_noalign_pearson <- kmap(
  x=x, y=y, n_clust = n_cluster, 
  warping_method = 'noalign', 
  similarity_method = 'pearson', 
  center_method = 'mean',
  fence=TRUE
)

kmap_show_results(fdakma_noalign_pearson)

x11()
par(mfrow=c(n_cluster,1))
for (i in seq(1,n_cluster )){
  clus = y[which(fdakma_noalign_pearson$labels == i),]
  time = x[which(fdakma_noalign_pearson$labels == i),]
  matplot(t(time),t(clus), type='l', xlab='x', ylab='orig.func', col = 'blue')
} 
x11()
matplot(t(time),t(uins), type='l', xlab='x', ylab='orig.func', col= fdakma_noalign_pearson$labels)
x11()
matplot(t(time),t(press), type='l', xlab='x', ylab='orig.func', col= fdakma_noalign_pearson$labels)

###
j = 1
for(i in unique(train_until1$breath_id))
{
  train_until1[which(train_until1$breath_id==i), 'clust_pearson'] = fdakma_noalign_pearson$labels[j]
  j = j+1
}
for(i in unique(test_until1$breath_id))
{
  test_until1[which(test_until1$breath_id==i), 'clust_pearson'] = fdakma_noalign_pearson$labels[j]
  j = j+1
}


x11()
plot(train_until1$u_in,train_until1$pressure, col=train_until1$clust )

x11()
plot(train_until1$tot_u_in, train_until1$pressure, col=train_until1$clust)

x11()
plot(ts_train$n_change_sign_u_in, ts_train$last_u_in, col=ts_train$clust)

cluster1<-ts_train[which(ts_train$clust==1),]
table(cluster1$R_C)
barplot(table(cluster1$R_C))


cluster2<-ts_train[which(ts_train$clust==2),]
table(cluster2$R_C)
barplot(table(cluster2$R_C))

cluster3<-ts_train[which(ts_train$clust==3),]
table(cluster3$R_C)
barplot(table(cluster3$R_C))

cluster11<-train_until1[which(train_until1$clust==1),]
cluster22<-train_until1[which(train_until1$clust==2),]
cluster33<-train_until1[which(train_until1$clust==3),]
write.table(cluster11, file="data_cluster1.csv", quote=T, sep=",", dec=".", na="NA", row.names=F, col.names=T)
write.table(cluster22, file="data_cluster2.csv", quote=T, sep=",", dec=".", na="NA", row.names=F, col.names=T)
write.table(cluster33, file="data_cluster3.csv", quote=T, sep=",", dec=".", na="NA", row.names=F, col.names=T)

