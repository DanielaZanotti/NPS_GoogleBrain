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

attach(train_s)

################
### CUMULATIVE SUM OF u_in 
################
 
train_s$tot_u_in <- 0
for (i in unique(train_s$breath_id)) {
  train_s[which(breath_id==i), 'tot_u_in'] <- cumsum(train_s[which(breath_id==i),6])
}


################
### FIRST AND LAST VALUE OF u_in
################

#since pressure would be getting closer to u_in in the end

train_s$last_u_in <- 0
for( i in seq(1,length(train_s$id),80))
{
  xx <- train_s[i+79, 6]
  id <- train_s[i+79, 2]
  train_s[which(breath_id==id), 'last_u_in'] <- xx
}

train_s$first_u_in <- 0
for( i in seq(1,length(train_s$id),80))
{
  xx <- train_s[i, 6]
  id <- train_s[i, 2]
  train_s[which(breath_id==id), 'first_u_in'] <- xx
}


###################
### MAX VALUE OF u_in FOR EACH BREATH
###################

train_s$max_u_in <- 0
for (i in unique(train_s$breath_id)) {
  mx <- max(train_s[which(breath_id==i), 6])
  train_s[which(breath_id==i), 'max_u_in'] <- mx
}


#####################
### DIFFERENCE BETWEEN CONSECUTIVE VALUES
#####################

train_s$u_in_diff1 <- 0
train_s$u_in_diff2 <- 0

for (i in unique(train_s$breath_id)) {
  shift <- c(0, 0, train_s[which(breath_id==i), 6])
  train_s[which(breath_id==i), 'u_in_diff1'] <- shift[3:82] - shift[2:81]
  train_s[which(breath_id==i), 'u_in_diff2'] <- shift[3:82] - shift[1:80]
}


################
### DIFFERENCE BETWEEN CURRENT VALUE OF u_in AND THE MAX VALUE WITHIN THE BREATH
################

train_s$u_in_diff_max <- 0
for (i in unique(train_s$breath_id)) {
  train_s[which(breath_id==i), 'u_in_diff_max'] <- train_s[which(breath_id==i), 6] - train_s[which(breath_id==i), 'max_u_in']
}


####################
### NUMBER OF CHANGE OF SIGN
####################

train_s$n_change_sign_u_in <- 0
for (i in unique(train_s$breath_id)) {
  x <- train_s[which(breath_id==i), 12]
  signs <- sign(x)
  ss <- sum(signs[-1] != signs[-length(x)])
  train_s[which(breath_id==i), 'n_change_sign_u_in'] <- ss
}















