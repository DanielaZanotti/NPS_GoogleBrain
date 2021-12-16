############################# LIBRARIES ##############################
library(roahd)
library(readr)
library(tidyverse) 
library(roahd)
library(fdakmapp)
library(data.table)
########################### LOAD DATA ################################
df = read.table("Data/train.csv",header=TRUE,sep=",")
head(df)
attach(df)

tab = fread("Data/train.csv")
tab_clus = tab[, .(u_in = u_in[1:30], time_step =  time_step[1:30]), by = .(breath_id)]

###################### FUNCTIONS SAMPLING ############################

# TRAINING SET

r_num <- c()
for( r in unique(df$R))
{
  for( c in unique(df$C))
  {
    d <- df[which(df$R==r & df$C==c),'breath_id']
    rn <- sample(unique(d), length(d)*0.00005, replace = F)
    r_num <- c(r_num, rn )
  }
}

# train_s: dataset con meno funzioni dell'originale estratte stratificando sulle categorie
train_s <- df %>% 
  filter(breath_id %in% r_num)

# train_until1 : dataset con solo le prime 30 osservazioni per ogni funzione, 
#dobbiamo prevedere la pressione solo in fase di inspirazione

train_until1=data.frame()
for(i in seq(1,length(train_s$id),80) ){
  t1=train_s[i:(i+29),]
  train_until1 = rbind(train_until1, t1)
}

# salvare i dati, e caricarli senza dover ogni volta caricare tutto il datset
write.table(train_until1, file="trainset.csv", quote=T, sep=",", dec=".", na="NA", row.names=F, col.names=T)
train_until1 = read.table("trainset.csv",header=TRUE,sep=",")

### TEST SET

test <- df

for( i in r_num)
{
  test <- test[which(test$breath_id != i), ]
}

r_num2 <- c()
for( r in unique(test$R))
{
  for( c in unique(test$C))
  {
    d <- test[which(test$R==r & test$C==c),'breath_id']
    rn2 <- sample(unique(d), length(d)*0.00005, replace = F)
    r_num2 <- c(r_num2, rn2 )
  }
}

test_s <- test %>% 
  filter(breath_id %in% r_num2)


test_until1 = data.frame()
for(i in seq(1,length(test_s$id),80) ){
  t1=test_s[i:(i+29),]
  test_until1 = rbind(test_until1, t1)
}

# salvare i dati, e caricarli senza dover ogni volta caricare tutto il datset

write.table(test_until1, file="testset.csv", quote=T, sep=",", dec=".", na="NA", row.names=F, col.names=T)
test_until1 = read.table("testset.csv",header=TRUE,sep=",")


### FUNCTIONAL DATA

#u_in
func_data1=data.frame()
for(i in seq(1,length(train_s$id),80) ){
  x1=train_s[i:(i+79),6]
  func_data1 = rbind(func_data1, t(x1))
}

grid <-  seq( 1, 80)
f_data1 <- fData(grid,func_data1)



#u_out
func_data2=data.frame()
for(i in seq(1,length(train_s$id),80) ){
  x2=train_s[i:(i+79),7]
  func_data2 = rbind(func_data2, t(x2))
}

grid <-  seq( 1, 80)
f_data2 <- fData(grid,func_data2)


#pressure 
func_data3=data.frame()
for(i in seq(1,length(train_s$id),80) ){
  x3=train_s[i:(i+79),8]
  func_data3 = rbind(func_data3, t(x3))
}

grid <-  seq( 1, 80)
f_data3 <- fData(grid,func_data3)


x11()
par(mfcol = c(1,3))
plot(f_data1,main="in")
plot(f_data2,main="out")
plot(f_data3,main="pressure")


#timestep
times=data.frame()
for(i in seq(1,length(train_s$id),80) ){
  tim=train_s[i:(i+79),5]
  times = rbind(times, t(tim))
}


###################### FUNCTIONS VISUALIZATION #######################
n=200
k=203

x11()
par(mfrow = c(k-n+1,3))
for (j in seq(n,k,1)){
  f1 <- fData(grid,func_data1[j,])

  f2 <- fData(grid,func_data2[j,])
  f3 <- fData(grid,func_data3[j,])
  plot(f1,main="in")
  plot(f2,main="out")
  plot(f3,main="pressure")
}

###################### FEATURE ENGINEERING ###################


### R_c

train_until1$R_C <- paste(as.character(train_until1$R),as.character(train_until1$C), sep ="_")
X<-split(train_until1, train_until1$R_C)
id_X = names(X)

test_until1$R_C <- paste(as.character(test_until1$R),as.character(test_until1$C), sep ="_")
X<-split(test_until1, test_until1$R_C)
id_X = names(X)


### CUMULATIVE SUM OF u_in 


train_until1$tot_u_in <- 0
for (i in unique(train_until1$breath_id)) {
  train_until1[which(train_until1$breath_id==i), 'tot_u_in'] <- cumsum(train_until1[which(train_until1$breath_id==i),6])
}

test_until1$tot_u_in <- 0
for (i in unique(test_until1$breath_id)) {
  test_until1[which(test_until1$breath_id==i), 'tot_u_in'] <- cumsum(test_until1[which(test_until1$breath_id==i),6])
}


### FIRST AND LAST VALUE OF u_in


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


test_until1$last_u_in <- 0
for( i in  unique(test_until1$breath_id))
{
  x = test_until1[which(test_until1$breath_id==i), ]
  l = dim(x)[1]
  test_until1[which(test_until1$breath_id==i), 'last_u_in'] <- x[l,'u_in']
}

test_until1$first_u_in <- 0
for( i in unique(test_until1$breath_id))
{
  x = test_until1[which(test_until1$breath_id==i), ]
  test_until1[which(test_until1$breath_id==i), 'first_u_in'] <- x[1,'u_in']
}



### MAX VALUE OF u_in FOR EACH BREATH


train_until1$max_u_in <- 0
for (i in unique(train_until1$breath_id)) {
  mx <- max(train_until1[which(train_until1$breath_id==i), 'u_in'])
  train_until1[which(train_until1$breath_id==i), 'max_u_in'] <- mx
}


test_until1$max_u_in <- 0
for (i in unique(test_until1$breath_id)) {
  mx <- max(test_until1[which(test_until1$breath_id==i), 'u_in'])
  test_until1[which(test_until1$breath_id==i), 'max_u_in'] <- mx
}



### DIFFERENCE BETWEEN CONSECUTIVE VALUES


train_until1$u_in_diff1 <- 0
train_until1$u_in_diff2 <- 0

for (i in unique(train_until1$breath_id)) {
  xx <- train_until1[which(train_until1$breath_id==i), 'u_in']
  l <- length(xx)
  shift <- c(0, 0, xx)
  train_until1[which(train_until1$breath_id==i), 'u_in_diff1'] <- shift[3:(l+2)] - shift[2:(l+1)]
  train_until1[which(train_until1$breath_id==i), 'u_in_diff2'] <- shift[3:(l+2)] - shift[1:l]
}


test_until1$u_in_diff1 <- 0
test_until1$u_in_diff2 <- 0

for (i in unique(test_until1$breath_id)) {
  xx <- test_until1[which(test_until1$breath_id==i), 'u_in']
  l <- length(xx)
  shift <- c(0, 0, xx)
  test_until1[which(test_until1$breath_id==i), 'u_in_diff1'] <- shift[3:(l+2)] - shift[2:(l+1)]
  test_until1[which(test_until1$breath_id==i), 'u_in_diff2'] <- shift[3:(l+2)] - shift[1:l]
}


### DIFFERENCE BETWEEN CURRENT VALUE OF u_in AND THE MAX VALUE WITHIN THE BREATH


train_until1$u_in_diff_max <- 0
for (i in unique(train_until1$breath_id)) {
  train_until1[which(train_until1$breath_id==i), 'u_in_diff_max'] <- train_until1[which(train_until1$breath_id==i), 'u_in'] - train_until1[which(train_until1$breath_id==i), 'max_u_in']
}

test_until1$u_in_diff_max <- 0
for (i in unique(test_until1$breath_id)) {
  test_until1[which(test_until1$breath_id==i), 'u_in_diff_max'] <- test_until1[which(test_until1$breath_id==i), 'u_in'] - test_until1[which(test_until1$breath_id==i), 'max_u_in']
}


### NUMBER OF CHANGE OF SIGN


train_until1$n_change_sign_u_in <- 0
for (i in unique(train_until1$breath_id)) {
  x <- train_until1[which(train_until1$breath_id==i), 'u_in_diff1']
  signs <- sign(x)
  ss <- sum(signs[-1] != signs[-length(x)])
  train_until1[which(train_until1$breath_id==i), 'n_change_sign_u_in'] <- ss
}

test_until1$n_change_sign_u_in <- 0
for (i in unique(test_until1$breath_id)) {
  x <- test_until1[which(test_until1$breath_id==i), 'u_in_diff1']
  signs <- sign(x)
  ss <- sum(signs[-1] != signs[-length(x)])
  test_until1[which(test_until1$breath_id==i), 'n_change_sign_u_in'] <- ss
}


######################## VISUALIZATION OF NEW VARIABLES #######################


ts_train = data.frame()
for(i in unique(train_until1$breath_id))
{
  x1 = train_until1[which(train_until1$breath_id==i), ]
  l = dim(x1)[1]
  ts_train = rbind(ts_train, x1[l,])
}

ts_test = data.frame()
for(i in unique(test_until1$breath_id))
{
  x1 = test_until1[which(test_until1$breath_id==i), ]
  l = dim(x1)[1]
  ts_test = rbind(ts_test, x1[l,])
}


x11()
pairs(ts_train[10:17])

###################### MULTIVARIATE CLUSTERING ################################
tr_te <- rbind(ts_train, ts_test)

cl.e <- dist(tr_te[10:17], method='euclidean')
cl.ew <- hclust(cl.e, method='ward')
plot(cl.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
rect.hclust(cl.ew, k=2)
cluster.ew <- cutree(cl.ew, k=2)


coph <- cophenetic(cl.ew)
cor(coph, cl.e)

j = 1
for(i in unique(train_until1$breath_id))
{
  train_until1[which(train_until1$breath_id==i), 'clust'] = cluster.ew[j]
  j = j+1
}
for(i in unique(test_until1$breath_id))
{
  test_until1[which(test_until1$breath_id==i), 'clust'] = cluster.ew[j]
  j = j+1
}

### VISUALIZATION

#time
func_data1=data.frame()
for(i in unique(train_until1$breath_id)  ){
  x1=train_until1[which(train_until1$breath_id==i),'time_step']
  func_data1 = rbind(func_data1, t(x1))
}

#pressure
func_data4=data.frame()
for(i in unique(train_until1$breath_id) ){
  x4=train_until1[which(train_until1$breath_id==i), 'pressure']
  func_data4 = rbind(func_data4, t(x4))
}

grid <-  seq( 1, 30)
f_data4 <- fData(grid,func_data4)

plot(f_data4,main="pressure")

#u_in
func_data2=data.frame()
for(i in unique(train_until1$breath_id) ){
  x2=train_until1[which(train_until1$breath_id==i), 'u_in']
  func_data2 = rbind(func_data2, t(x2))
}

grid <-  seq( 1, 30)
f_data2 <- fData(grid,func_data2)

plot(f_data2,main="in")

#clust
func_data5 = data.frame()
for(i in unique(train_until1$breath_id)){
  x5 = train_until1[which(train_until1$breath_id==i),'clust']
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





###################### FUNCTIONAL CLUSTERING ###########################


library(fdakmapp)

tt <- rbind(train_until1, test_until1)

#time
time = data.frame()
for(i in unique(tt$breath_id)  ){
  x1=tt[which(tt$breath_id==i),'time_step']
  time = rbind(time, t(x1))
}

#u_in
uin = data.frame()
for(i in unique(tt$breath_id)  ){
  x1=tt[which(tt$breath_id==i),'u_in']
  uin = rbind(uin, t(x1))
}

#pressure
pres = data.frame()
for(i in unique(tt$breath_id) ){
  x4=tt[which(tt$breath_id==i), 'pressure']
  pres = rbind(pres, t(x4))
}

times = as.matrix(time)
uins = as.matrix(uin)
press = as.matrix(pres)

# Plot of original functions
matplot(t(times), t(uins), type='l', xlab='time step', ylab='u in')
title ('Original functions')

x <- c()
y <- c()
for (i in unique(tt$breath_id))
{
  t <- tt[which(tt$breath_id==i), 'time_step']
  u <- tt[which(tt$breath_id==i), 'u_in']
  x <- rbind(x, t)
  y <- rbind(y, u)
}



set.seed(2304)

n_cluster=3

fdakma_shift <- kmap(
  x=x, y=y, n_clust = n_cluster, 
  warping_method= 'shift',
  similarity_method = 'l2', 
  center_method = 'mean',
  fence=TRUE
)

kmap_show_results(fdakma_shift)

x11()
par(mfrow=c(3,n_cluster/3))
for (i in seq(1,n_cluster )){
  clus = y[which(fdakma_shift$labels == i),]
  time = x[which(fdakma_shift$labels == i),]
  matplot(t(time),t(clus), type='l', xlab='x', ylab='orig.func', col= "grey")
} 
matplot(t(time),t(uins), type='l', xlab='x', ylab='orig.func', col= fdakma_shift$labels)
matplot(t(time),t(press), type='l', xlab='x', ylab='orig.func', col= fdakma_shift$labels)


fdakma_affine <- kmap(
  x=x, y=y, n_clust = n_cluster, 
  warping_method = 'affine', 
  similarity_method = 'l2', 
  center_method = 'mean'
)

kmap_show_results(fdakma_affine)

x11()
par(mfrow=c(3,n_cluster/3))
for (i in seq(1,n_cluster )){
  clus = y[which(fdakma_affine$labels == i),]
  time = x[which(fdakma_affine$labels == i),]
  matplot(t(time),t(clus), type='l', xlab='x', ylab='orig.func', col= "grey")
} 
matplot(t(time),t(uins), type='l', xlab='x', ylab='orig.func', col= fdakma_affine$labels)
matplot(t(time),t(press), type='l', xlab='x', ylab='orig.func', col= fdakma_affine$labels)


fdakma_noalign <- kmap(
  x=x, y=y, n_clust = n_cluster, 
  warping_method = 'noalign', 
  similarity_method = 'l2', 
  center_method = 'mean',
  fence=TRUE
)

kmap_show_results(fdakma_noalign)

x11()
par(mfrow=c(3,n_cluster/3))
for (i in seq(1,n_cluster )){
  clus = y[which(fdakma_noalign$labels == i),]
  time = x[which(fdakma_noalign$labels == i),]
  matplot(t(time),t(clus), type='l', xlab='x', ylab='orig.func', col= "grey")
} 
matplot(t(time),t(uins), type='l', xlab='x', ylab='orig.func', col= fdakma_noalign$labels)
matplot(t(time),t(press), type='l', xlab='x', ylab='orig.func', col= fdakma_noalign$labels)

###
j = 1
for(i in unique(train_until1$breath_id))
{
  train_until1[which(train_until1$breath_id==i), 'clust'] = fdakma_noalign$labels[j]
  j = j+1
}
for(i in unique(test_until1$breath_id))
{
  test_until1[which(test_until1$breath_id==i), 'clust'] = fdakma_noalign$labels[j]
  j = j+1
}


x11()
plot(train_until1$u_in,train_until1$pressure, col=train_until1$clust )

x11()
plot(train_until1$tot_u_in, train_until1$pressure, col=train_until1$clust)

x11()
plot(ts_train$n_change_sign_u_in, ts_train$last_u_in, col=ts_train$clust)

cluster1<-ts_train[which(ts_train$clust==1),]
table(cluster1$R_C)
barplot(table(cluster1$R_C))


cluster2<-ts_train[which(ts_train$clust==2),]
table(cluster2$R_C)
barplot(table(cluster2$R_C))

cluster3<-ts_train[which(ts_train$clust==3),]
table(cluster3$R_C)
barplot(table(cluster3$R_C))

cluster11<-train_until1[which(train_until1$clust==1),]
cluster22<-train_until1[which(train_until1$clust==2),]
cluster33<-train_until1[which(train_until1$clust==3),]
write.table(cluster11, file="data_cluster1.csv", quote=T, sep=",", dec=".", na="NA", row.names=F, col.names=T)
write.table(cluster22, file="data_cluster2.csv", quote=T, sep=",", dec=".", na="NA", row.names=F, col.names=T)
write.table(cluster33, file="data_cluster3.csv", quote=T, sep=",", dec=".", na="NA", row.names=F, col.names=T)

############################### DEPTH MEASURES ###########################

#depths
band_depth <- BD(Data = f_data1)
modified_band_depth <- MBD(Data = f_data1)

#median
median_curve <- median_fData(fData = f_data1, type = "MBD") 

#spearman correlation
bivariate_data <- as.mfData(list(f_data1, f_data3))
plot(bivariate_data)
cor_spearman(bivariate_data, ordering='MHI')

#outlier detection
invisible(fbplot(f_data1))
invisible(outliergram(f_data1))

#outliers labels
out_shape <- outliergram(f_data1, display = FALSE)
out_shape$ID_outliers


################################ MIXED EFFECTS GAM ####################################


library(gamm4)
library(RLRsim)
fm1 <- gamm4(pressure ~ s(u_in) + R + C + s(time_step)+ u_out,
             data=train_s,
             random = ~ (u_in + time_step| breath_id))

summary(fm1$gam) ## summary of gam
summary(fm1$mer) ## underlying mixed model



