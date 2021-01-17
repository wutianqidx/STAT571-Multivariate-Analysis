#Problem 2
mydata = read.csv("schooltest.csv")
n1 = 13
n2 = 12
q = 6
Y1 = mydata[1:13,3:8]
Y2 = mydata[14:25,3:8]
sigma = (t(Y1)%*%scale(Y1,scale=F) + t(Y2)%*%scale(Y2,scale=F))/(n1+n2-2)
Y1_bar = as.matrix(1/n1 * t(rep(1, n1))) %*% as.matrix(Y1)
Y2_bar = as.matrix(1/n2 * t(rep(1, n2))) %*% as.matrix(Y2)
Z = solve(sqrt(1/n1+1/n2)) %*% (Y1_bar-Y2_bar)
Z
W = (n1 + n2-2) * sigma
W
T2= (n1 + n2-2)*Z%*%solve(W)%*%t(Z)
T2
F = (n1+n2-1-q)/(q*(n1+n2-2))*T2
F
p = 1 - pf(F,q,n1+n2-1-q)
p

#Problem 3
library(msos)
coastal = read.csv("coastalcities.csv",header = T)
Y = as.matrix(coastal[,-c(1:3)])
tmp = cbind(coastal$Latitude^0, coastal$Latitude^1, coastal$Latitude^2,coastal$Latitude^3)
x = qr.Q(qr(tmp))
z = cbind(1,cos(1:12*2*pi/12),sin(1:12*2*pi/12))
bsm = bothsidesmodel(x,Y,z)
bsmh1 = bothsidesmodel.hotelling(x,Y,z,rows=4,cols=1:3)
bsm$Beta[4,]
bsmh1$Hotelling$T2
bsmh1$Hotelling$F
bsmh1$Hotelling$pvalue

bsmh2 = bothsidesmodel.hotelling(x,Y,z,rows=3:4,cols=2:3)
bsm$Beta[3:4,2:3]
bsmh2$Hotelling$T2
bsmh2$Hotelling$F
bsmh2$Hotelling$pvalue

#Problem 4
data("caffeine")
Y = as.matrix(caffeine[,-1])
x = cbind(caffeine[,1]^0,caffeine[,1]^1,caffeine[,1]^2)
z = cbind(1,c(-1,1))

cps = NULL
for(l in (1:2)) for(p in (1:3)) {
  pattern = matrix(0,ncol=2,nrow=3)
  pattern[1:p,1:l] = 1
  bsm = bothsidesmodel(x,Y,z,pattern)
  cps = rbind(cps,c(l,p,bsm$ResidSS,bsm$Dim,bsm$Cp))
}
df = data.frame(cps)
colnames(df)[1:5]=c("l*","p*","ResidSS*","d*","Cp*")
df
