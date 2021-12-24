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

mu=colMeans(t_set)
res=c_set-mu
ncm=apply(res,2,max)
ncm_sort=c(sort(ncm),Inf)
d=ncm_sort[ceiling((n/2 + 1)*(1-alpha))]

matplot(cbind(mu,mu+d,mu-d),type='l')


#scale the amplitude
S=apply(t_set,2,var)
res=(c_set-mu)/S
ncm=apply(res,2,max)
ncm_sort=c(sort(ncm),Inf)
d=ncm_sort[ceiling((n/2 + 1)*(1-alpha))]

matplot(cbind(mu,mu+(d*S),mu-(d*S)),type='l')