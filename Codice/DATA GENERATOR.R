############################# LIBRARIES ##############################
library(roahd)
library(readr)
library(tidyverse) 
library(roahd)
library(fdakmapp)
library(data.table)


###################### DATA GENERATION ###################################


#read the dataset as a data.frame
df = read.table("Data/train.csv",header=TRUE,sep=",")
head(df)
attach(df)

#read the dataset as a data.table
tab = fread("Data/train.csv")

##################### DATA FOR CLUSTERING ###################################

# sampling indices for trainingset stratifying on R and C
r_num <- c()
for( r in unique(df$R))
{
  for( c in unique(df$C))
  {
    d <- unique(df[which(df$R==r & df$C==c),'breath_id'])
    rn <- sample(unique(d), length(d)*0.1, replace = F)
    r_num <- c(r_num, rn )
  }
}

# sampling
train_s_tab <- tab %>% 
  filter(breath_id %in% r_num)

#tronco il dataset a 30 osservazioni
train_until1_tab = train_s_tab[, .(id=id[1:30], R=R[1:30], C=C[1:30],time_step =  time_step[1:30], u_in = u_in[1:30] , u_out= u_out[1:30], pressure=pressure[1:30]), by = .(breath_id)]

train_until1_clust<-data.frame(train_until1_tab)

# salvare i dati, e caricarli senza dover ogni volta caricare tutto il datset
write.table(train_until1_clust, file="trainset_clustering.csv", quote=T, sep=",", dec=".", na="NA", row.names=F, col.names=T)



#metto in riga le u_in e i time step

n=length(unique(train_until1_clust$breath_id))
u_in_row=data.frame(matrix(0,n,30))
i=1
f= function(vect){
  u_in_row[i,]<<-t(vect)
  i<<-i+1
}


func_data1=train_until1_tab[,f(u_in),by=breath_id] 

write.table(u_in_row, file="u_in_row.csv", quote=T, sep=",", dec=".", na="NA", row.names=F, col.names=T)


i=1
time_step_row=data.frame(matrix(0,n,30))
g= function(vect){
  time_step_row[i,]<<-t(vect)
  i<<-i+1
}

times=train_until1_tab[,g(time_step),by=breath_id] 

write.table(time_step_row, file="time_step_row.csv", quote=T, sep=",", dec=".", na="NA", row.names=F, col.names=T)



#################### DATA FOR FEATURE ENGINEERING AND GAMM ###########################################

#train_until1<-train_until1_clust[1:18000,]

j = 1
for(i in unique(train_until1_clust$breath_id))
{
  train_until1_clust[which(train_until1_clust$breath_id==i), 'clust'] = fdakma_pearson$labels[j]
  j = j+1
}

r_num2 <- c()
for( r in unique(train_until1_clust$R))
{
  for( c in unique(train_until1_clust$C))
  {
    d <- unique(train_until1_clust[which(train_until1_clust$R==r & train_until1_clust$C==c),'breath_id'])
    rn2 <- sample(unique(d), length(d)*0.05, replace = F)
    r_num2 <- c(r_num2, rn2 )
  }
}

# sampling
train_until1 <- train_until1_clust %>% 
  filter(breath_id %in% r_num2)

write.table(train_until1, file="training_set_feature.csv", quote=T, sep=",", dec=".", na="NA", row.names=F, col.names=T)
