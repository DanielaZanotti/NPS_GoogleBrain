############################# LIBRARIES ########################################
library(roahd)
library(readr)
library(tidyverse) 
library(roahd)
library(fdakmapp)
library(data.table)


###################### DATA GENERATION #########################################

# Read the dataset as a data.frame

df = read.table("Data/train.csv",header=TRUE,sep=",")
head(df)
attach(df)

# Read the dataset as a data.table
tab = fread("Data/train.csv")


##################### CUT THE DATASET AT 30 OBSERVATIONS #######################

data = tab[, .(id = id[1:30], R = R[1:30], C = C[1:30], time_step = time_step[1:30],
                   u_in = u_in[1:30] , u_out= u_out[1:30], pressure = pressure[1:30]), 
                   by = .(breath_id)]

data = data.frame(data)

# Save the data

write.table(data, file="Data/data.csv", 
            quote=T, sep=",", dec=".", na="NA", row.names=F, col.names=T)

