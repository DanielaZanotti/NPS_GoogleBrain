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
medians = read.table("Data/medians.csv",header=TRUE,sep=",")


# GAM for each cluster

gam_models = list()
R2 = c()
rc = c()

for(i in 1:length(unique(train_final$clust))){
  
  clus = train_final[which(train_final$clust == i),]
  rc = rbind(rc, clus$R_C)
  
  model <- gam(pressure ~ time_step + s(u_in) + s(tot_u_in) + s(area) +
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

i = 45
summary(gam_models[[i]]) 
plot(gam_models[[i]], ylim = c(-20, 20))

hist(gam_models[[i]]$residuals, xlab='Residuals')
qqnorm(gam_models[[i]]$residuals)
qqline(gam_models[[i]]$residuals)

shapiro.test(gam_models[[i]]$residuals[1:5000])

pp = predict(gam_models[[i]])
plot(pp, residuals(gam_models[[i]]), xlab = 'Prediction', ylab = 'Residuals')

