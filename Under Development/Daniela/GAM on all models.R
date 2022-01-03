################################ LIBRARIES ######################
library(roahd)
library(readr)
library(tidyverse) 
library(roahd)
library(fdakmapp)
library(data.table)
library(gamm4)
library(knitr)
library(mgcv)
library(fdakma)

############################### LOAD DATA ########################
train_final = read.table("Data/trainset_GAMM_full_clust.csv",header=TRUE,sep=",")
medians = read.table("Data/medians.csv",header=TRUE,sep=",")

#Create training set
d <- unique(train_final[,'breath_id'])
rn <- sample(d, length(d)*0.80, replace = F)

# Sampling
train <- train_final %>% 
  filter(breath_id %in% rn)

# Create test set
test  <- anti_join(train_final, train, by = 'id')


gam_models = c()
R2 = c()
for( i in unique(train$clust))
{
  clus = train[which(train$clust == i),]
  
  model <- gam(pressure ~ time_step + s(u_in) + s(tot_u_in) +
              + s(u_in_diff1) + s(u_in_diff2) + s(u_in_diff3) + s(u_in_diff4) + s(u_in_diff5) +
              + s(u_in_shift1) + s(u_in_shift2) + s(u_in_shift3) + 
              + s(u_in_shift4) + s(u_in_shift5) +
              + factor(R_C),
            data=clus)
  
  S = summary(model)
  R2 = rbind(R2, S$r.sq)
  gam_models = rbind(gam_models, model)
  
}

mean(R2)
min(R2)
max(R2)


# New obs
d2 <- unique(test[,'breath_id'])
rn2 <- sample(d2, 1, replace = F)
new_obs <- test %>% 
  filter(breath_id %in% rn2)

grid = seq(0, 1, by=1/29)
max = 0
idx = 0
for( i in 1:dim(medians)[1])
{
  pears = kma.similarity(x.f=new_obs$time_step, y0.f=new_obs$u_in, 
                         x.g=grid, y0.g=medians[i, ], similarity.method='d0.pearson')
  if( pears > max)
  {
    max = pears
    idx = i
  }
}

max
idx
new_obs$clust

plot(new_obs$time_step, new_obs$u_in, type='l')
lines(grid, medians[idx,], col='red')
lines(grid, medians[new_obs$clust[1],], col='blue')
