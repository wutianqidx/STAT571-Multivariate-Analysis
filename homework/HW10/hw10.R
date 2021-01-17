library(MASS)
library(msos)
data("crabs")
crabs
x.crabs = as.matrix(crabs[,4:8])
y.crabs = rep(1:4,c(50,50,50,50))
ld.crabs = lda(x.crabs,y.crabs)
ld.crabs$a
ld.crabs$c
disc = x.crabs%*%ld.crabs$a
disc = sweep(disc,2,ld.crabs$c,'+')
imax = function(z) ((1:length(z))[z==max(z)])[1]
yhat = apply(disc,1,imax)
table(yhat,y.crabs)
ClassError = (5+3)/200
ClassError
yhat.cv = NULL
varin=1:5
n = nrow(x.crabs)
for(i in 1:n) {
  dcv = lda(x.crabs[-i,varin],y.crabs[-i])
  dxi = x.crabs[i,varin]%*%dcv$a+dcv$c
  yhat.cv = c(yhat.cv,imax(dxi))
}
sum(yhat.cv!=y.crabs)/n

#3
data("SAheart")
heartfull = glm(chd~.,data=SAheart,family=binomial)
summary(heartfull)
heartstepb = step(heartfull,scope=list(upper= ~.,lower = ~1),k=log(nrow(SAheart)),trace=0)
heartfactor = glm(chd ~ tobacco+ldl+adiposity+obesity+alcohol,data=SAheart,family=binomial)
heartfull.BIC = heartfull$deviance + log(462)*10
heartfull.yhat = ifelse(predict(heartfull)>0,1,0)
sum(heartfull.yhat!=SAheart[,'chd'])/462
heartfull.err = NULL
for(i in 1:462) {
  yfiti = glm(chd ~., family = binomial,data = SAheart,subset=(1:462)[-i])
  dhati = predict(yfiti,newdata=SAheart[i,])
  yhati = ifelse(dhati>0,1,0)
  heartfull.err = c(heartfull.err,sum(yhati!=SAheart[i,'chd']))
}
mean(heartfull.err)

heartstepb.BIC = heartstepb$deviance+log(462)*6
heartstepb.yhat = ifelse(predict(heartstepb)>0,1,0)
sum(heartstepb.yhat!=SAheart[,'chd'])/462
heartstepb.err = NULL
for(i in 1:462) {
  yfiti = glm(chd ~., family = binomial,data = SAheart,subset=(1:462)[-i])
  stepi = step(yfiti,scope=list(upper= ~.,lower = ~1),k=log(462),trace=0)
  dhati = predict(stepi,newdata=SAheart[i,])
  yhati = ifelse(dhati>0,1,0)
  heartstepb.err = c(heartstepb.err,sum(yhati!=SAheart[i,'chd']))
}

heartfactor.BIC = heartfactor$deviance + log(462)*6
heartfactor.yhat = ifelse(predict(heartfactor)>0,1,0)
sum(heartfactor.yhat!=SAheart[,'chd'])/462
heartfactor.err = NULL
for(i in 1:462) {
  yfiti = glm(chd ~tobacco+ldl+adiposity+obesity+alcohol, 
              family = binomial,data = SAheart,subset=(1:462)[-i])
  dhati = predict(yfiti,newdata=SAheart[i,])
  yhati = ifelse(dhati>0,1,0)
  heartfactor.err = c(heartfactor.err,sum(yhati!=SAheart[i,'chd']))
}
mean(heartfactor.err)

#4
