help("read.table")
library(roahd)
data = read.table("train.csv",header=TRUE,sep=",")
head(data)
attach(data)

#pressure in
func_data1=data.frame()
j=1
for(i in seq(1,6640,80) ){
  x1=data[i:(i+79),6]
  func_data1[j,1:80]=x1
  j=j+1
}

grid <-  seq( 1, 80)
f_data1 <- fData(grid,func_data1)



#pressure out
func_data2=data.frame()
j=1
for(i in seq(1,6640,80) ){
  x2=data[i:(i+79),7]
  func_data2[j,1:80]=x2
  j=j+1
}

grid <-  seq( 1, 80)
f_data2 <- fData(grid,func_data2)



#pressure 
func_data3=data.frame()
j=1
for(i in seq(1,6640,80) ){
  x3=data[i:(i+79),8]
  func_data3[j,1:80]=x3
  j=j+1
}

grid <-  seq( 1, 80)
f_data3 <- fData(grid,func_data3)
x11()
par(mfcol = c(1,3))
plot(f_data1,main="in")
plot(f_data2,main="out")
plot(f_data3,main="pressure")


