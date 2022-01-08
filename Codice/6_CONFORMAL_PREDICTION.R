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
library(conformalInference)

set.seed(2304)

################################ CONFORMAL PREDICTION ##########################

validation = read.table("Data/validationset.csv",header=TRUE,sep=",")

alpha = 0.1

pred_valid = c()
pres_row_valid = c()

K = c()
S = c()

for( i in 1:length(unique(validation$clust)))
{
  clus = validation[which(validation$clust == i),]
  
  # Prediction for cluster i
  pred_gam = predict(gam_models[[i]], newdata= clus)
  pred_gam = matrix(pred_gam, ncol = 30 , byrow = TRUE)
  
  pred_valid = rbind(pred_valid, pred_gam)
  
  pres_row = c()
  for(i in unique(clus$breath_id)){
    p = clus[which(clus$breath_id==i), 'pressure']
    pres_row = rbind(pres_row, p)
  }
  pres_row_valid = rbind(pres_row_valid, pres_row)
  
  n=length(unique(clus$breath_id))
  
  # Compute bands for cluster i 
  res=abs(pres_row - pred_gam)
  ncm = apply(res,2,max)
  ncm_sort=c(sort(ncm))
  m = min(n, 30)
  k = ncm_sort[ceiling((m/2 + 1)*(1-alpha))]
  x = seq(0, 1, by=1/29)
  line_integral = function(x, y) {
    dx <- diff(x)
    end <- length(y)
    my <- (y[1:(end - 1)] + y[2:end]) / 2
    sum(dx *my)
  } 
  den = line_integral(x, ncm)
  s = ncm/den
  
  K = rbind(K, k)
  S = rbind(S, s)
  
}


i = 23
plot(grid, pres_row[i,], type = "l", col ="red", ylim = c(-10,30))
lines(grid,  pres_row[i,]+(K[2]*S[2,]), type='l', lty =2)
lines(grid,  pres_row[i,]-(K[2]*S[2,]), type='l', lty =2)
lines(grid, pred_gam[i,], type='l', col='blue')






