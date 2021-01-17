library(msos)

#Problem 4 
mydata = read.csv("coastalcities.csv")
x_second = rep(1, 12)
x_third = rep(1, 12)
for (i in 1:length(x_second)){
  x_second[i]=cos(2*i/12*pi)
  x_third[i]=sin(2*i/12*pi)
}

y = mydata[,4:15]
x = matrix(c(rep(1,31),mydata[,3],mydata[,3]^2), ncol = 3)
qx = qr.Q(qr(x))
z = matrix(c(rep(1,12),x_second,x_third), ncol = 3)
bsm = bothsidesmodel(qx,y,z)
bsm$Beta
bsm$SE
bsm$T

#Problem 3
library(msos)
data("grades")
y1 = grades[,2:6]
x_second1 = rep(1, 107)
for (i in 1:107){
  if (grades[i,1] == 1)
    x_second1[i] = 0.37
  else
    x_second1[i] = -0.7
}
x1 = matrix(c(rep(1,107),x_second1), ncol = 2)
z1 = cbind(1,c(2,2,2,-3,-3),c(1,1,-2,0,0),c(1,-1,0,0,0),c(0,0,0,1,-1))
cx = solve(t(x1) %*% x1)
bsm1 = bothsidesmodel(x1,y1,z1)
cx
#(b)
bsm1$Beta
#(c)
bsm1$Sigmaz
bsm1$SE
bsm1$T
