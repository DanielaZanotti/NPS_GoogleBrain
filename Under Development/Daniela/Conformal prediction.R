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

################################ CONFORMAL PREDICTION ######################

n=length(unique(clus1$breath_id))
pres_row=c()
for(i in unique(clus1$breath_id)){
  p <- clus1[which(clus1$breath_id==i), 'pressure']
  pres_row <- rbind(pres_row, p)
}

alpha = 0.1

i1=sample(1:n,n/2)
t_set=pres_row[i1,]
c_set=pres_row[-i1,]

grid <-  seq( 1, 30)
t_set_func <- fData(grid,t_set)
median_curve <- median_fData(fData = t_set_func, type = "MBD")

#mu=colMeans(t_set)
mu = median_curve$values[1,]
res=c_set-mu
ncm=apply(res,2,max)
ncm_sort=c(sort(ncm),Inf)
m <- min(n, 30)
d=ncm_sort[ceiling((m/2 + 1)*(1-alpha))]

matplot(cbind(mu,mu+d,mu-d),type='l')

i = 31
plot(test$time_step[((i-1)*30 + 1):(i*30)], mu, type = "l", col ="red", ylim = c(-30, 60))
lines(test$time_step[((i-1)*30 + 1):(i*30)], mu+d, type='l')
lines(test$time_step[((i-1)*30 + 1):(i*30)], mu-d, type='l')
lines(test$time_step[((i-1)*30 + 1):(i*30)],pred_gam[((i-1)*30 + 1):(i*30)], col='blue')


#scale the amplitude
S=apply(t_set,2,var)
SS <- sqrt(S)
res=(c_set-mu)/S
ncm=apply(res,2,max)
ncm_sort=c(sort(ncm),Inf)
m <- min(n, 30)
d=ncm_sort[ceiling((m/2 + 1)*(1-alpha))]

matplot(cbind(mu,mu+(d*S),mu-(d*S)),type='l')

matplot(cbind(mu,mu+(d*SS),mu-(d*SS)),type='l')

i = 37
plot(test$time_step[((i-1)*30 + 1):(i*30)],mu, type = "l", col ="red", ylim = c(-40, 40))
lines(test$time_step[((i-1)*30 + 1):(i*30)], mu+(d*SS), type='l')
lines(test$time_step[((i-1)*30 + 1):(i*30)], mu-(d*SS), type='l')
lines(test$time_step[((i-1)*30 + 1):(i*30)],pred_gam[((i-1)*30 + 1):(i*30)], col='blue')



