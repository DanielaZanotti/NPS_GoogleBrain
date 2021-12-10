library(fdakma)

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

fdakma_shift <- kma(
  x=x, y0=y, n.clust = 10, 
  warping.method = 'shift', 
  similarity.method = 'd0.L2', 
  center.method = 'k-means',
  show.iter = TRUE,
  fence = TRUE
  #,seeds = c(1,11,21) # you can give a little help to the algorithm...
)

kma.show.results(fdakma_shift)


fdakma_affine <- kma(
  x=x, y0=y, n.clust = 3, 
  warping.method = 'affine', 
  similarity.method = 'd0.pearson', 
  center.method = 'k-means',
  show.iter = TRUE,
  t.max = 0.05,
  m.max = 0.05
  #fence = TRUE
  #,seeds = c(1,11,21) # you can give a little help to the algorithm...
)
kma.show.results(fdakma_affine)
