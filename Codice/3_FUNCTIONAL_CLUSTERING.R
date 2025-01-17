#################### LIBRARIES #################################################

library(roahd)
library(readr)
library(tidyverse) 
library(roahd)
library(fdakmapp)
library(data.table)
library(fda)

set.seed(2304)

##################### FUNCTIONAL CLUSTERING ####################################

data = read.table("Data/data_feature.csv",header=TRUE,sep=",")

# Create training and test sets

d = unique(data[,'breath_id'])
r_num = sample(d, length(d)*0.80, replace = F)

train <- data %>% 
  filter(breath_id %in% r_num)

test  = anti_join(data, train, by = 'id')

# Create matrix of u_in and time step 

n = length(unique(train$breath_id))
y = c()
x = c()
for (i in unique(train$breath_id))
{
  temp1 = train[which(train$breath_id==i), 'u_in']
  temp2 = train[which(train$breath_id==i), 'time_step']
  y = rbind(y, temp1)
  x = rbind(x, temp2)
}

#y = as.matrix(u_in_row)
#x = as.matrix(time_step_row)


############################# SILHOUETTE ANALYSIS ##############################

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


############################ PEARSON CLUSTERING ################################

n_cluster=50

fdakma_pearson_50_dil <- kmap(
  x=x, y=y, n_clust = n_cluster,
  warping_method = 'dilation',
  similarity_method = 'pearson',
  center_method = 'mean',
  fence=TRUE
)

kmap_show_results(fdakma_pearson_50_dil)

x11()
par(mfrow=c(5,5))
for (i in seq(1, 25)){
  clus = y[which(fdakma_pearson_50_dil$labels == i),]
  time = x[which(fdakma_pearson_50_dil$labels == i),]
  idx = data.frame(seq(1,dim(clus)[1] ))
  curr_idx = idx %>% sample_frac(0.10)
  clus = clus[curr_idx[,1],]
  time = time[curr_idx[,1],]
  matplot(t(time),t(clus), type='l', xlab='x', ylab='orig.func', col= "blue")
}


# Add the clusters in the dataset

j = 1
for(i in unique(train$breath_id))
{
  train[which(train$breath_id==i), 'clust'] = fdakma_pearson_50_dil$labels[j]
  j = j+1
}

# Save the data

write.table(train, file="Data/train_cluster.csv", 
            quote=T, sep=",", dec=".", na="NA", row.names=F, col.names=T)

write.table(test, file="Data/testset.csv", 
            quote=T, sep=",", dec=".", na="NA", row.names=F, col.names=T)




