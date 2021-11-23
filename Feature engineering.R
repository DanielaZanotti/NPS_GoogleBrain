library(readr)
library(tidyverse) 
library(roahd)
library(fdakmapp)
set.seed(2304)

df <-  read.table("Data/train.csv",header=TRUE,sep=",")

num <- length(df$breath_id)
r_num <- sample(unique(df$breath_id), num*0.0005, replace = F)
length(r_num)

train_s <- df %>% 
  filter(breath_id %in% r_num)

train_until1 <- train_s %>% 
  filter(train_s$u_out == 0)

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
  train_until1[which(breath_id==i), 'tot_u_in'] <- cumsum(train_until1[which(breath_id==i),6])
}


################
### FIRST AND LAST VALUE OF u_in
################

train_until1$last_u_in <- 0
for( i in  unique(train_until1$breath_id))
{
  x = train_until1[which(breath_id==i), ]
  l = dim(x)[1]
  train_until1[which(breath_id==i), 'last_u_in'] <- x[l,'u_in']
}

train_until1$first_u_in <- 0
for( i in unique(train_until1$breath_id))
{
  x = train_until1[which(breath_id==i), ]
  train_until1[which(breath_id==i), 'first_u_in'] <- x[1,'u_in']
}


###################
### MAX VALUE OF u_in FOR EACH BREATH
###################

train_until1$max_u_in <- 0
for (i in unique(train_until1$breath_id)) {
  mx <- max(train_until1[which(breath_id==i), 'u_in'])
  train_until1[which(breath_id==i), 'max_u_in'] <- mx
}


#####################
### DIFFERENCE BETWEEN CONSECUTIVE VALUES
#####################

train_until1$u_in_diff1 <- 0
train_until1$u_in_diff2 <- 0

for (i in unique(train_until1$breath_id)) {
  xx <- train_until1[which(breath_id==i), 'u_in']
  l <- length(xx)
  shift <- c(0, 0, xx)
  train_until1[which(breath_id==i), 'u_in_diff1'] <- shift[3:(l+2)] - shift[2:(l+1)]
  train_until1[which(breath_id==i), 'u_in_diff2'] <- shift[3:(l+2)] - shift[1:l]
}


################
### DIFFERENCE BETWEEN CURRENT VALUE OF u_in AND THE MAX VALUE WITHIN THE BREATH
################

train_until1$u_in_diff_max <- 0
for (i in unique(train_until1$breath_id)) {
  train_until1[which(breath_id==i), 'u_in_diff_max'] <- train_until1[which(breath_id==i), 'u_in'] - train_until1[which(breath_id==i), 'max_u_in']
}


####################
### NUMBER OF CHANGE OF SIGN
####################

train_until1$n_change_sign_u_in <- 0
for (i in unique(train_until1$breath_id)) {
  x <- train_until1[which(breath_id==i), 'u_in_diff1']
  signs <- sign(x)
  ss <- sum(signs[-1] != signs[-length(x)])
  train_until1[which(breath_id==i), 'n_change_sign_u_in'] <- ss
}



###### plot
func_data1=data.frame()
for(i in seq(1,length(train_s$id),80) ){
  x1=train_s[i:(i+79),6]
  func_data1 = rbind(func_data1, t(x1))
}

grid <-  seq( 1, 80)
f_data1 <- fData(grid,train_s[which(breath_id==392), 'u_in'])

plot(f_data1)








