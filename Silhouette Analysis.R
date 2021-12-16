library(fdakmapp)
train_until1 = read.table("trainset.csv",header=TRUE,sep=",")
test_until1 = read.table("testset.csv",header=TRUE,sep=",")

tab = fread("Data/train.csv")
tab_clus = tab[, .(u_in = u_in[1:30], time_step =  time_step[1:30]), by = .(breath_id)]
tt <- data.frame(tab_clus[breath_id<= 5200])

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

compute_wss = function(clus_out, x,y,k){
  
  wss.c = 0
  for(j in seq(1,k)){
   
    medoid_y = clus_out$y.centers.final[j,1:97,1]
    medoid_x = clus_out$x.centers.final[1:97]
    
    #estraggo i breath nel cluster
    selected_ids = which(clus_out$labels == j)
    #prendo le righe corrispondenti a quel breath
    for(sel in selected_ids){
      if(sum(y[sel,]) == 0 ){
        next
      }
      sim = (1 - kma.similarity(x.f = medoid_x, y0.f = medoid_y,
                           x.g =x[sel,] , y0.g = y[sel,]  , similarity.method = "d0.pearson", unif.grid = TRUE))^2
      
      wss.c = wss.c + sim 
      wss.c
      
      
    }
    
  }
  return(wss.c)
}


n_cluster=30
wss = c()
for(i in seq(1,n_cluster) ){
  print(i)
  m = c()
  for(rip in seq(1,5)){
    fdakma_noalign_pearson <- kmap(
      x=x, y=y, n_clust = i, 
      warping_method = 'noalign', 
      similarity_method = 'pearson', 
      center_method = 'mean',
      fence=TRUE
    )
    wss.curr = compute_wss(fdakma_noalign_pearson,x,y,i)
    m = c(m,wss.curr)
  }
  
  print(wss.curr)
  wss = c(wss,mean(m))
  
  
}



