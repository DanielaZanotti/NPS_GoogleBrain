
library(data.table)
library(roahd)
data = read.table("/Users/leopere/Desktop/MTM/Magistrale/Secondo_anno/Primo_Semestre/NonParam/NPS_GoogleBrain/Data/train.csv",header=TRUE,sep=",")

#select number of breaths and set the grid
n_breath = 6640
grid <-  seq( 1, 80)

#pressure in
p_in=data.frame(matrix(NA, nrow = n_breath, ncol = 80))
#pressure out
p_out=data.frame(matrix(NA, nrow = n_breath, ncol = 80))
#pressure 
y=data.frame(matrix(NA, nrow = n_breath, ncol = 80))
j=1
for(i in seq(1,n_breath,80) ){
  
  p_in[j,1:80] = data[i:(i+79),6]
  p_out[j,1:80] = data[i:(i+79),7]
  y[j,1:80] = data[i:(i+79),8]
  
  j=j+1
}


p_in_fun <- fData(grid,p_in)
p_out_fun <- fData(grid,p_out)
y_fun <- fData(grid,y)



par(mfcol = c(1,3))
plot(p_in_fun,main="in")
plot(p_out_fun,main="out")
plot(y_fun,main="pressure")





#Depth measures and outlier detection

band_depth <- BD(Data = p_in_fun)
modified_band_depth <- MBD(Data = p_in_fun)
median_curve <- median_fData(fData = p_in_fun, type = "MBD")
fbplot(p_in_fun, main="Magnitude outliers")
outliergram(p_in_fun)

























