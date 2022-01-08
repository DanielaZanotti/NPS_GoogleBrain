############################## LIBRARIES #######################################

library(roahd)
library(readr)
library(tidyverse) 
library(roahd)
library(fdakmapp)
library(data.table)

################################# LOAD DATA ####################################

data = read.table("Data/data.csv",header=TRUE,sep=",")

############################### FEATURE ENGINEERING ############################

################
### R_C
################

data$R_C = paste(as.character(data$R),as.character(data$C), sep ="_")
X = split(data, data$R_C)
id_X = names(X)


################
### CUMULATIVE SUM OF u_in 
################

data$tot_u_in = 0
for (i in unique(data$breath_id)) {
  data[which(data$breath_id==i), 'tot_u_in'] = cumsum(data[which(data$breath_id==i),6])
}


################
### FIRST AND LAST VALUE OF u_in
################

data$last_u_in = 0
for( i in  unique(data$breath_id))
{
  x = data[which(data$breath_id==i), ]
  l = dim(x)[1]
  data[which(data$breath_id==i), 'last_u_in'] = x[l,'u_in']
}

data$first_u_in = 0
for( i in unique(data$breath_id))
{
  x = data[which(data$breath_id==i), ]
  data[which(data$breath_id==i), 'first_u_in'] = x[1,'u_in']
}


###################
### MAX VALUE OF u_in FOR EACH BREATH
###################

data$max_u_in = 0
for (i in unique(data$breath_id)) {
  mx <- max(data[which(data$breath_id==i), 'u_in'])
  data[which(data$breath_id==i), 'max_u_in'] = mx
}


#####################
### DIFFERENCE BETWEEN CONSECUTIVE VALUES
#####################

data$u_in_diff1 = 0
data$u_in_diff2 = 0
data$u_in_diff3 = 0
data$u_in_diff4 = 0
data$u_in_diff5 = 0

for (i in unique(data$breath_id)) {
  xx = data[which(data$breath_id==i), 'u_in']
  l = length(xx)
  shift = c(0, 0, 0, 0, 0, xx)
  data[which(data$breath_id==i), 'u_in_diff1'] = shift[6:(l+5)] - shift[5:(l+4)]
  data[which(data$breath_id==i), 'u_in_diff2'] = shift[6:(l+5)] - shift[4:(l+3)]
  data[which(data$breath_id==i), 'u_in_diff3'] = shift[6:(l+5)] - shift[3:(l+2)]
  data[which(data$breath_id==i), 'u_in_diff4'] = shift[6:(l+5)] - shift[2:(l+1)]
  data[which(data$breath_id==i), 'u_in_diff5'] = shift[6:(l+5)] - shift[1:l]
}


################
### DIFFERENCE BETWEEN CURRENT VALUE OF u_in AND THE MAX VALUE WITHIN THE BREATH
################

data$u_in_diff_max = 0
for (i in unique(data$breath_id)) {
  data[which(data$breath_id==i), 'u_in_diff_max'] = data[which(data$breath_id==i), 'u_in'] - data[which(data$breath_id==i), 'max_u_in']
}


####################
### NUMBER OF CHANGE OF SIGN
####################

data$n_change_sign_u_in = 0
for (i in unique(data$breath_id)) {
  x = data[which(data$breath_id==i), 'u_in_diff1']
  signs = sign(x)
  ss = sum(signs[-1] != signs[-length(x)])
  data[which(data$breath_id==i), 'n_change_sign_u_in'] = ss
}


###################
### AREA
###################

data$area = 0

for (i in unique(data$breath_id)) {
  u = data[which(data$breath_id==i), 'u_in']
  tmp = data[which(data$breath_id==i), 'time_step']
  l = length(tmp)
  shift = c(0, tmp)
  data[which(data$breath_id==i), 'area'] = (shift[2:(l+1)] - shift[1:l])*u
}


#####################
### SHIFT u_in
#####################

data$u_in_shift1 = 0
data$u_in_shift2 = 0
data$u_in_shift3 = 0
data$u_in_shift4 = 0
data$u_in_shift5 = 0

for (i in unique(data$breath_id)) {
  xx = data[which(data$breath_id==i), 'u_in']
  l = length(xx)
  shift = c(xx[1], xx[1], xx[1], xx[1], xx[1], xx)
  data[which(data$breath_id==i), 'u_in_shift1'] = shift[5:(l+4)]
  data[which(data$breath_id==i), 'u_in_shift2'] = shift[4:(l+3)]
  data[which(data$breath_id==i), 'u_in_shift3'] = shift[3:(l+2)]
  data[which(data$breath_id==i), 'u_in_shift4'] = shift[2:(l+1)]
  data[which(data$breath_id==i), 'u_in_shift5'] = shift[1:l]
}


#######################
######### CLUSTERING
######################

j = 1
for(i in unique(data$breath_id))
{
  data[which(data$breath_id==i), 'clust'] = fdakma_pearson$labels[j]
  j = j+1
}


# Save the data

write.table(data, file="Data/data_feature.csv", 
            quote=T, sep=",", dec=".", na="NA", row.names=F, col.names=T)


