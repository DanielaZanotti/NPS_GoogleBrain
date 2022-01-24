################################ LIBRARIES ####################################
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

########################## DEAL WITH A NEW OBSERVATION #########################

test = read.table("Data/testset.csv",header=TRUE,sep=",")

# New observation 

d3 = unique(test[,'breath_id'])
rn3 = sample(d3, 1, replace = F)
new_obs = test %>% 
  filter(breath_id %in% rn3)


# Assign the obs to a cluster using pearson similarity

grid = seq(0, 1, by=1/29)
max = 0
idx = 0

possible_groups = c()
for (i in 1:dim(rc)[1])
{
  if(new_obs$R_C[1] %in% unique(rc[i,]))
  {
    possible_groups = c(possible_groups, i)
  }
}

for( i in possible_groups)
{
  pears = kma.similarity(x.f=new_obs$time_step, y0.f=new_obs$u_in, 
                         x.g=grid, y0.g=medians[i, ], similarity.method='d0.pearson')
  if ( is.nan(pears) )
  {
    pears = 0
  }
  if( pears > max)
  {
    max = pears
    idx = i
  }
}

plot(new_obs$time_step, new_obs$u_in, type='l')
lines(grid, medians[idx,], col='red')


# Predict the pressure
if (idx == 0)
{
  prediction = rep(0, 30)
}

prediction = predict(gam_models[[idx]], newdata= new_obs)


# Plot the true pressure, the predicted pressure and the conformal prediction 

x11()
plot(new_obs$time_step,new_obs$pressure, type = "l", col ="black",ylim =c(-25,80), xlab = 'Time stamp', ylab = 'Pressure')
lines(new_obs$time_step,prediction, col="red")
lines(new_obs$time_step, new_obs$pressure+(K[idx]*S[idx,]), type='l', lty =2)
lines(new_obs$time_step, new_obs$pressure-(K[idx]*S[idx,]), type='l', lty =2)



