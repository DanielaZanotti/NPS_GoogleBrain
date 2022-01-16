################################ LIBRARIES #####################################
library(roahd)
library(readr)
library(tidyverse) 
library(roahd)
library(fdakmapp)
library(data.table)
library(gamm4)
library(knitr)
library(mgcv)
library(fdatest)

set.seed(2304)

############################### LOAD DATA ######################################

train = read.table("Data/train_cluster.csv",header=TRUE,sep=",")

# Interval Wise Testing among all the pairs of clusters

tst = c()
for(i in seq(1, 49))
{
  clus1 <- train[which(train$clust == i),]
  clus1_u_in <- matrix(clus1$u_in, ncol = 30 , byrow = TRUE)
  
  for(j in seq(i+1, 50))
  {
    clus2 <- train[which(train$clust == j),]
    clus2_u_in <- matrix(clus2$u_in, ncol = 30 , byrow = TRUE)
    temp = IWT2(clus1_u_in,clus2_u_in)
    temp$i = i
    temp$j = j
    tst = rbind(tst, temp)
  }
}

# Detection of similar pairs 

same = data.frame()
k=1
for( i in seq(1, 1225))
{
  ap <- tst[i, 'adjusted_pval']
  if( length( which( ap[[1]] > 0.05)) > 0.70*30)
  {
    same[k, 1] = tst[i, 'i'][[1]]
    same[k, 2] = tst[i, 'j'][[1]]
    same[k, 3] = length( which( ap[[1]] > 0.05))
    k=k+1
  }
}
colnames(same) = c('i', 'j', 'len')


grid <- seq(0,1, by=1/29)

# Join the clusters

train_final = train

compare = function(i, j){
  clus1 <- train_final[which(train_final$clust == i),]
  clus1_u_in <- matrix(clus1$u_in, ncol = 30 , byrow = TRUE)
  clus2 <- train_final[which(train_final$clust == j),]
  clus2_u_in <- matrix(clus2$u_in, ncol = 30 , byrow = TRUE)
  temp = IWT2(clus1_u_in,clus2_u_in)
  ap <- temp$adjusted_pval
  if( length( which( ap[[1]] > 0.05)) > 0.70*30)
    {return(TRUE)}
  return(FALSE)
}

same = same[order(same$len, decreasing = T),] 
s = seq(1, 50)
for(k in 1:dim(same)[1])
{ 
  comp = compare(s[same[k, 'i']], s[same[k, 'j']])
  
  if(s[same[k, 'i']]==same[k, 'i'] & s[same[k, 'j']]==same[k, 'j'])
  {
    train_final[which(train_final$clust==same[k, 'j']),'clust'] = same[k, 'i']
    s[which(s == same[k, 'j'])] = same[k, 'i']
  }
  else if(s[same[k, 'i']]==same[k, 'i'] & s[same[k, 'j']]!=same[k, 'j'] & comp)
  {
    train_final[which(train_final$clust==s[same[k, 'j']]),'clust'] = same[k, 'i']
    s[which(s == same[k, 'j'])] = same[k, 'i']
  }
  else if(s[same[k, 'i']]!=same[k, 'i'] & s[same[k, 'j']]==same[k, 'j']  & comp)
  {
    train_final[which(train_final$clust==same[k, 'j']),'clust'] = s[same[k, 'i']]
    s[which(s == same[k, 'j'])] = s[same[k, 'i']]
  }
  else if(s[same[k, 'i']]!=same[k, 'i'] & s[same[k, 'j']]!=same[k, 'j']  & comp)
  {
    train_final[which(train_final$clust==s[same[k, 'j']]),'clust'] = s[same[k, 'i']]
    s[which(s == same[k, 'j'])] = s[same[k, 'i']]
  }
}


n = 1
for(z in unique(s))
{
  train_final[which(train_final$clust== z),'clust'] = n
  n = n+1
}


# Compute the median of each clusters with depth measures

medians = c()
for(i in seq(1, n-1))
{
  clus1 = train_final[which(train_final$clust == i),]
  clus1_u_in <- matrix(clus1$u_in, ncol = 30 , byrow = TRUE)
  
  funct1 <- fData(grid,clus1_u_in)
  median_curve <- median_fData(fData = funct1, type = "MBD")
  
  medians = rbind(medians, median_curve$values)
}


write.table(train_final, file="Data/train_final.csv", quote=T, sep=",", dec=".", na="NA", row.names=F, col.names=T)
write.table(medians, file="Data/medians.csv", quote=T, sep=",", dec=".", na="NA", row.names=F, col.names=T)
