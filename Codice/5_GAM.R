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

set.seed(2304)

############################### LOAD DATA ########################

train_final = read.table("Data/train_final.csv",header=TRUE,sep=",")
validation = read.table("Data/validationset.csv",header=TRUE,sep=",")
medians = read.table("Data/medians.csv",header=TRUE,sep=",")


# GAM for each cluster

gam_models = list()
R2 = c()

for(i in 1:length(unique(validation$clust))){
  
  clus = validation[which(validation$clust == i),]
  
  model <- gam(pressure ~ time_step + s(u_in) + s(tot_u_in) +
                 + s(u_in_diff1) + s(u_in_diff2) + s(u_in_diff3) + s(u_in_diff4) + s(u_in_diff5) +
                 + s(u_in_shift1) + s(u_in_shift2) + s(u_in_shift3) + 
                 + s(u_in_shift4) + s(u_in_shift5) +
                 + factor(R_C),
               data=clus)
  
  S = summary(model)
  R2 = rbind(R2, S$r.sq)
  gam_models[[i]] =  model
}

mean(R2)
min(R2)
max(R2)

# Example of summary and residuals of a model

summary(gam_models[[6]]) 
plot(gam_models[[6]], ylim = c(-10, 10))

hist(gam_models[[6]]$residuals)
qqnorm(gam_models[[6]]$residuals)
qqline(gam_models[[6]]$residuals)

