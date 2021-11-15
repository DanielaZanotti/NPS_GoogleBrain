############################# LIBRARIES ##############################
library(roahd)
library(readr)
library(tidyverse) 
library(roahd)
library(fdakmapp)
########################### LOAD DATA ################################
df = read.table("train.csv",header=TRUE,sep=",")
head(data)
attach(data)


###################### FUNCTIONS SAMPLING ############################


num <- length(df$breath_id)
r_num <- sample(unique(df$breath_id), num*0.0005, replace = F)
length(r_num)

train_s <- df %>% 
  filter(breath_id %in% r_num)

attach(train_s)

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



###################### CLUSTERING ###########################
times2=as.matrix(times)
ff=as.matrix(func_data1)

fdakma_example <- kmap(
  x=times2, y = ff, n_clust = 20, 
  warping_method = 'affine', 
  similarity_method = 'pearson',  # similarity computed as the cosine
  # between the first derivatives 
  # (correlation)
  center_method = 'mean'
 
)

kmap_show_results(fdakma_example)

clus1 = ff[which(fdakma_example$labels == 1), 1:80]
time1 = times2[which(fdakma_example$labels == 1), 1:80]
data_W.fd.1 <- Data2fd(y = clus1,argvals = time1,basisobj = basis.1)

x11()
matplot(t(time1),t(clus1), type='l', xlab='x', ylab='orig.func', col= "grey")


clus3 = ff[which(fdakma_example$labels == 5), 1:80]
time3 = times2[which(fdakma_example$labels == 5), 1:80]
x11()
matplot(t(time3),t(clus3), type='l', xlab='x', ylab='orig.func', col= "grey")





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


