############################## LIBRARIES ####################

library(roahd)
library(readr)
library(tidyverse) 
library(roahd)
library(fdakmapp)
library(data.table)
################################# LOAD DATA #####################################
train_until1 = read.table("Data/trainining_set_feature.csv",header=TRUE,sep=",")

############################### FEATURE ENGINEERING ###############################

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
  train_until1[which(train_until1$breath_id==i), 'tot_u_in'] <- cumsum(train_until1[which(train_until1$breath_id==i),6])
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
train_until1$u_in_diff3 <- 0

for (i in unique(train_until1$breath_id)) {
  xx <- train_until1[which(train_until1$breath_id==i), 'u_in']
  l <- length(xx)
  shift <- c(0, 0, 0, xx)
  train_until1[which(train_until1$breath_id==i), 'u_in_diff1'] <- shift[4:(l+3)] - shift[3:(l+2)]
  train_until1[which(train_until1$breath_id==i), 'u_in_diff2'] <- shift[4:(l+3)] - shift[2:(l+1)]
  train_until1[which(train_until1$breath_id==i), 'u_in_diff3'] <- shift[4:(l+3)] - shift[1:l]
}






################
### DIFFERENCE BETWEEN CURRENT VALUE OF u_in AND THE MAX VALUE WITHIN THE BREATH
################

train_until1$u_in_diff_max <- 0
for (i in unique(train_until1$breath_id)) {
  train_until1[which(train_until1$breath_id==i), 'u_in_diff_max'] <- train_until1[which(train_until1$breath_id==i), 'u_in'] - train_until1[which(train_until1$breath_id==i), 'max_u_in']
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



###################
### AREA
###################

train_until1$area <- 0

for (i in unique(train_until1$breath_id)) {
  u <- train_until1[which(train_until1$breath_id==i), 'u_in']
  tmp <- train_until1[which(train_until1$breath_id==i), 'time_step']
  l <- length(tmp)
  shift <- c(0, tmp)
  train_until1[which(train_until1$breath_id==i), 'area'] <- (shift[2:(l+1)] - shift[1:l])*u
}


#######################
######### CLUSTERING
######################

j = 1
for(i in unique(train_until1$breath_id))
{
  train_until1[which(train_until1$breath_id==i), 'clust'] = fdakma_pearson$labels[j]
  j = j+1
}


#### save
write.table(train_until1, file="trainset_GAMM.csv", quote=T, sep=",", dec=".", na="NA", row.names=F, col.names=T)


