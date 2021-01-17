library(msos)
data("caffeine")
y = as.matrix(caffeine[,-1])
x = cbind(1,c(rep(-1,9),rep(0,10),rep(1,9)),c(rep(1,9),rep(-9/5,10),rep(1,9)))
z = matrix(c(1,-1,1,1),nrow = 2,byrow = T)
results = NULL
models = NULL
for(p in (1:3)) for(l in (1:2)) {
  pattern = matrix(0,ncol = 2, nrow = 3)
  pattern[1:p,1:l] = 1
  bothsidesmodel = bothsidesmodel.mle(x,y,z,pattern)
  results = c(p,l,bothsidesmodel$Dev,bothsidesmodel$Dim,bothsidesmodel$BIC)
  models = rbind(models,results)
}
bic = models[,5]
p = exp(-(bic-max(bic))/2)
p = 100*p/sum(p)
final = cbind(models,p)
colnames(final) = c("p*","l*","Deviance","Dimension"," BIC ","probability")
final
best_pattern = cbind(c(1,1,0),c(1,1,0))
best_bothsidesmodel = bothsidesmodel.mle(x,y,z,best_pattern)
best_bothsidesmodel$Beta
