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

times = as.matrix(time)
uins = as.matrix(uin)

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

  similarity_method = 'pearson', 
  center_method = 'mean',

  #,seeds = c(1,11,21) # you can give a little help to the algorithm...
)

kmap_show_results(fdakma_shift)

x11()
par(mfrow=c(3,n_cluster/3))
for (i in seq(1,n_cluster )){
  clus = y[which(fdakma_shift$labels == i),1:30]
  time = x[which(fdakma_shift$labels == i),1:30]
  matplot(t(time),t(clus), type='l', xlab='x', ylab='orig.func', col= "grey")
  
} 


