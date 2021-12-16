library(fdakmapp)
library(purrr)
train_until1 = read.table("trainset.csv",header=TRUE,sep=",")
test_until1 = read.table("testset.csv",header=TRUE,sep=",")

tab = fread("Data/train.csv")
tab_clus = tab[, .(u_in = u_in[1:30], time_step =  time_step[1:30]), by = .(breath_id)]
tt <- data.frame(tab_clus[breath_id<= 5200])

f = function(vect,tab){
  tab <<- rbind(tab, t(vect))
}

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



#Silhoutte Analysis
set.seed(2304)

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
#Abbiamo scelto 25 cluster
write.csv(wss, "wss.csv")

fdakma_noalign_pearson <- kmap(
  x=x, y=y, n_clust = 25, 
  warping_method = 'noalign', 
  similarity_method = 'pearson', 
  center_method = 'mean',
  fence=TRUE
)

table(fdakma_noalign_pearson$labels)

x11()
par(mfrow=c(5,5))
for (i in seq(1,n_cluster )){
  clus = y[which(fdakma_noalign_pearson$labels == i),]
  time = x[which(fdakma_noalign_pearson$labels == i),]
  matplot(t(time),t(clus), type='l', xlab='x', ylab='orig.func', col= "grey")
} 
i = 11
x11()
clus = y[which(fdakma_noalign_pearson$labels == i),]
time = x[which(fdakma_noalign_pearson$labels == i),]
matplot(t(time),t(clus), type='l', xlab='x', ylab='orig.func', col= "grey")



