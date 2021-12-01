library(readr)
library(tidyverse) 
library(roahd)
library(fdakmapp)
set.seed(2304)
library(mgcv)

df <-  read.table("Data/train.csv",header=TRUE,sep=",")

num <- length(df$breath_id)
r_num <- sample(unique(df$breath_id), num*0.00005, replace = F)
length(r_num)

train_s <- df %>% 
  filter(breath_id %in% r_num)

#train_until1 <- train_s %>% 
#  filter(train_s$u_out == 0)

train_until1=data.frame()
for(i in seq(1,length(train_s$id),80) ){
  t1=train_s[i:(i+29),]
  train_until1 = rbind(train_until1, t1)
}


attach(train_until1)

detach(train_until1)




################
### R_c
################
train_until1$R_C <- paste(as.character(train_until1$R),as.character(train_until1$C), sep ="_")
X<-split(train_until1, train_until1$R_C)
id_X = names(X)

################
### CUMULATIVE SUM OF u_in 
################
 
train_until1$tot_u_in <- 0
for (i in unique(train_until1$breath_id)) {
  train_until1[which(train_until1$breath_id==i), 'tot_u_in'] <- cumsum(train_until1[which(breath_id==i),6])
}


################
### FIRST AND LAST VALUE OF u_in
################

train_until1$last_u_in <- 0
for( i in  unique(train_until1$breath_id))
{
  x = train_until1[which(train_until1$breath_id==i), ]
  l = dim(x)[1]
  train_until1[which(train_until1$breath_id==i), 'last_u_in'] <- x[l,'u_in']
}

train_until1$first_u_in <- 0
for( i in unique(train_until1$breath_id))
{
  x = train_until1[which(train_until1$breath_id==i), ]
  train_until1[which(train_until1$breath_id==i), 'first_u_in'] <- x[1,'u_in']
}


###################
### MAX VALUE OF u_in FOR EACH BREATH
###################

train_until1$max_u_in <- 0
for (i in unique(train_until1$breath_id)) {
  mx <- max(train_until1[which(train_until1$breath_id==i), 'u_in'])
  train_until1[which(train_until1$breath_id==i), 'max_u_in'] <- mx
}


#####################
### DIFFERENCE BETWEEN CONSECUTIVE VALUES
#####################

train_until1$u_in_diff1 <- 0
train_until1$u_in_diff2 <- 0

for (i in unique(train_until1$breath_id)) {
  xx <- train_until1[which(train_until1$breath_id==i), 'u_in']
  l <- length(xx)
  shift <- c(0, 0, xx)
  train_until1[which(train_until1$breath_id==i), 'u_in_diff1'] <- shift[3:(l+2)] - shift[2:(l+1)]
  train_until1[which(train_until1$breath_id==i), 'u_in_diff2'] <- shift[3:(l+2)] - shift[1:l]
}


################
### DIFFERENCE BETWEEN CURRENT VALUE OF u_in AND THE MAX VALUE WITHIN THE BREATH
################

train_until1$u_in_diff_max <- 0
for (i in unique(train_until1$breath_id)) {
  train_until1[which(train_until1$breath_id==i), 'u_in_diff_max'] <- train_until1[which(train_until1$breath_id==i), 'u_in'] - train_until1[which(breath_id==i), 'max_u_in']
}


####################
### NUMBER OF CHANGE OF SIGN
####################

train_until1$n_change_sign_u_in <- 0
for (i in unique(train_until1$breath_id)) {
  x <- train_until1[which(train_until1$breath_id==i), 'u_in_diff1']
  signs <- sign(x)
  ss <- sum(signs[-1] != signs[-length(x)])
  train_until1[which(train_until1$breath_id==i), 'n_change_sign_u_in'] <- ss
}



###### plot
func_data1=data.frame()
for(i in seq(1,length(train_s$id),80) ){
  x1=train_s[i:(i+79),6]
  func_data1 = rbind(func_data1, t(x1))
}

grid <-  seq( 1, 80)

f_data1 <- fData(grid,train_s[which(train_s$breath_id==392), 'u_in'])

plot(f_data1)



################
### VISUALIZATION OF NEW VARIABLES
################

ts = data.frame()
for(i in unique(train_until1$breath_id))
{
  x1 = train_until1[which(train_until1$breath_id==i), ]
  l = dim(x1)[1]
  ts = rbind(ts, x1[l,])
}

x11()
pairs(ts[10:17])


################
### CLUSTERING OF NEW VARIABLES
################

### 2 CLUSTERS
cl.e <- dist(ts[10:17], method='euclidean')
cl.ew <- hclust(cl.e, method='ward')
plot(cl.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(cl.ew, k=2)
cluster.ew <- cutree(cl.ew, k=2)


coph <- cophenetic(cl.ew)
cor(coph, cl.e)

j = 1
for(i in seq(1,length(train_s$id),80) ){
  train_s[i:(i+79),'clust'] = cluster.ew[j]
  j = j +1
}

#time
func_data1=data.frame()
for(i in unique(train_s$breath_id)  ){
  x1=train_s[which(train_s$breath_id==i),'time_step']
  func_data1 = rbind(func_data1, t(x1))
}

#pressure
func_data4=data.frame()
for(i in unique(train_s$breath_id) ){
  x4=train_s[which(train_s$breath_id==i), 'pressure']
  func_data4 = rbind(func_data4, t(x4))
}

grid <-  seq( 1, 80)
f_data4 <- fData(grid,func_data4)



#u_in
func_data2=data.frame()
for(i in unique(train_s$breath_id) ){
  x2=train_s[which(train_s$breath_id==i), 'u_in']
  func_data2 = rbind(func_data2, t(x2))
}

grid <-  seq( 1, 80)
f_data2 <- fData(grid,func_data2)


#clust
func_data5 = data.frame()
for(i in unique(train_s$breath_id)){
  x5 = train_s[which(train_s$breath_id==i),'clust']
  func_data5 = rbind(func_data5, t(x5))
}

times = as.matrix(func_data1)
pr = as.matrix(func_data4)
uin = as.matrix(func_data2)
clu = as.matrix(func_data5)

x11()
par(mfrow=c(1,2))
matplot(t(times),t(pr), type='l', xlab='x', ylab='orig.func', col= clu)
matplot(t(times),t(uin), type='l', xlab='x', ylab='orig.func', col= clu)


################
### PCA OF NEW VARIABLES
################

variab=ts[,10:17]
variab.sd <- scale(variab)
variab.sd <- data.frame(variab.sd)


#PRINCIPAL COMPONENTS
pca <- princomp(variab.sd, scores=T)
summary(pca)


d=pca$scores

cl.e <- dist(d, method='euclidean')
cl.ew <- hclust(cl.e, method='ward')
plot(cl.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(cl.ew, k=3)
cluster.ew <- cutree(cl.ew, k=3)




j = 1
for(i in seq(1,length(train_s$id),80) ){
  train_s[i:(i+79),'clust'] = cluster.ew[j]
  j = j +1
}


#clust
func_data5 = data.frame()
for(i in unique(train_s$breath_id)){
  x5 = train_s[which(train_s$breath_id==i),'clust']
  func_data5 = rbind(func_data5, t(x5))
}

times = as.matrix(func_data1)
pr = as.matrix(func_data4)
uin = as.matrix(func_data2)
clu = as.matrix(func_data5)

x11()
par(mfrow=c(1,2))
matplot(t(times),t(pr), type='l', xlab='x', ylab='orig.func', col= clu)
matplot(t(times),t(uin), type='l', xlab='x', ylab='orig.func', col= clu)




################################
attach(train_s)
         
rm(ls = fm)
fm<-gamm(pressure ~ s(u_in) + s(time_step) + R + C + u_out ,random=list(breath_id=~1),data=train_s)
summary(fm$lme)
summary(fm$gam)

library(gamm4)
fm1<-gamm4(pressure ~ s(u_in) + s(time_step) + R + C + u_out ,data=train_s,random = ~ (u_in+time_step|breath_id))


help(gamm4)
summary(fm1$gam) ## summary of gam
summary(fm1$mer) ## underlying mixed model
