#################
### CLUSTERING
################


tr_te <- rbind(ts_train, ts_test)

cl.e <- dist(tr_te[10:17], method='euclidean')
cl.ew <- hclust(cl.e, method='ward')
plot(cl.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(cl.ew, k=2)
cluster.ew <- cutree(cl.ew, k=3)


coph <- cophenetic(cl.ew)
cor(coph, cl.e)


j = 1
for(i in unique(train_until1$breath_id))
{
  train_until1[which(train_until1$breath_id==i), 'clust'] = cluster.ew[j]
  j = j+1
}
for(i in unique(test_until1$breath_id))
{
  test_until1[which(test_until1$breath_id==i), 'clust'] = cluster.ew[j]
  j = j+1
}


##################
### VISUALIZATION
##################
#time
func_data1=data.frame()
for(i in unique(train_until1$breath_id)  ){
  x1=train_until1[which(train_until1$breath_id==i),'time_step']
  func_data1 = rbind(func_data1, t(x1))
}

#pressure
func_data4=data.frame()
for(i in unique(train_until1$breath_id) ){
  x4=train_until1[which(train_until1$breath_id==i), 'pressure']
  func_data4 = rbind(func_data4, t(x4))
}

grid <-  seq( 1, 30)
f_data4 <- fData(grid,func_data4)

plot(f_data4,main="pressure")

#u_in
func_data2=data.frame()
for(i in unique(train_until1$breath_id) ){
  x2=train_until1[which(train_until1$breath_id==i), 'u_in']
  func_data2 = rbind(func_data2, t(x2))
}

grid <-  seq( 1, 30)
f_data2 <- fData(grid,func_data2)

plot(f_data2,main="in")

#clust
func_data5 = data.frame()
for(i in unique(train_until1$breath_id)){
  x5 = train_until1[which(train_until1$breath_id==i),'clust']
  func_data5 = rbind(func_data5, t(x5))
}

times = as.matrix(func_data1)
pr = as.matrix(func_data4)
uin = as.matrix(func_data2)
clu = as.matrix(func_data5)

x11()
par(mfrow=c(1,2))
matplot(t(times),t(pr), type='l', xlab='x', ylab='orig.func', col= clu)
matplot(t(times),t(uin), type='l', xlab='x', ylab='orig.func', col= clu)


#################
### CLUSTERING ad albero
################

c1 <- which(cluster.ew == 1)
c2 <- which(cluster.ew == 2)

plot(ts[c1, 'max_u_in'])
cl.e <- dist(ts[c1, 'max_u_in'], method='euclidean')
cl.ew <- hclust(cl.e, method='ward')
plot(cl.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(cl.ew, k=2)
cluster.ew <- cutree(cl.ew, k=2)

plot(ts[c1, 'max_u_in'], col = cluster.ew)

plot(ts[c2, 'max_u_in'])
cl.e <- dist(ts[c2, 'max_u_in'], method='euclidean')
cl.ew <- hclust(cl.e, method='ward')
plot(cl.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(cl.ew, k=2)
cluster.ew2 <- cutree(cl.ew, k=2)

plot(ts[c2, 'tot_u_in'], col = cluster.ew2)


j = 1
for(i in c1 ){
  idx = ts[i, 'breath_id']
  train_until1[which(train_until1$breath_id==idx),'clust'] = cluster.ew[j]
  j = j +1
}
j = 1
for(i in c2 ){
  idx = ts[i, 'breath_id']
  train_until1[which(train_until1$breath_id==idx),'clust'] = cluster.ew2[j]+2
  j = j +1
}






