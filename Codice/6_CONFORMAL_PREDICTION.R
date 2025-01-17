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
library(fdakma)
library(conformalInference)

set.seed(2304)

################################ CONFORMAL PREDICTION ##########################

# Assign the obs of the testset to a cluster using pearson similarity

grid = seq(0, 1, by=1/29)
nn = 0
for (k in unique(test$breath_id)) {
  
  new_obs = test[which(test$breath_id==k), ]
  
  possible_groups = c()
  for (i in 1:dim(rc)[1])
  {
    if(new_obs$R_C[1] %in% unique(rc[i,]))
    {
      possible_groups = c(possible_groups, i)
    }
  }
  
  max = 0
  idx = 0
  for( i in possible_groups)
  {
    pears = kma.similarity(x.f=new_obs$time_step, y0.f=new_obs$u_in, 
                           x.g=grid, y0.g=medians[i, ], similarity.method='d0.pearson')
    if ( is.nan(pears) )
    {
      nn = nn + 1
      pears = 0
    }
    if( pears > max)
    {
      max = pears
      idx = i
    }
  }
  test[which(test$breath_id==k),'pred_clust'] = idx
}

# Conformal prediction 

alpha = 0.1

pred_cp = c()
pres_row_cp = c()

K = c()
S = c()

d2 = unique(test[,'breath_id'])
rn2 = sample(d2, 100, replace = F)
new_observation = test %>% 
  filter(breath_id %in% rn2)
calibration_set = anti_join(test, new_observation, by='id')

for( i in 1:(length(unique(test$pred_clust))-1))
{
  clus = calibration_set[which(calibration_set$pred_clust == i),]

  # Prediction for cluster i
  pred_gam = predict(gam_models[[i]], newdata= clus)
  pred_gam = matrix(pred_gam, ncol = 30 , byrow = TRUE)
  
  pred_cp = rbind(pred_cp, pred_gam)
  
  pres_row = matrix(clus[,'pressure'], ncol = 30 , byrow = TRUE)
  pres_row_cp = rbind(pres_row_cp, pres_row)
  
  # Compute bands for cluster i 
  res = abs(pres_row - pred_gam)
  ncm = apply(res,2,max)
  ncm_sort = c(sort(ncm))
  m = min(nr/2, 30)
  k = ncm_sort[ceiling((m/2 + 1)*(1-alpha))]
  x = seq(0, 1, by=1/29)
  line_integral = function(x, y) {
    dx = diff(x)
    end = length(y)
    my = (y[1:(end - 1)] + y[2:end]) / 2
    sum(dx *my)
  } 
  den = line_integral(x, ncm)
  s = ncm/den
  
  K = rbind(K, k)
  S = rbind(S, s)
  
}


totres = abs(pres_row_cp - pred_cp)
RSS = sum(totres^2) 
TSS = sum((pres_row_cp - mean(pred_cp))^2)

R2 = 1 - (RSS/TSS)


