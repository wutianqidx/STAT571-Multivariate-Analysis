library(msos)
#1
school = read.csv("schooltest.csv",header=T)
Y = school[,-(1:2)]
Bigten = as.matrix(subset(Y,school$PAC12 == 0))
Pac12 = as.matrix(subset(Y,school$PAC12 == 1))
dfbig = nrow(Bigten)-1
dfpac = nrow(Pac12)-1
dfbig
dfpac

covbig = cov(Bigten)
covpac = cov(Pac12)
covpool=(covbig*dfbig+covpac*dfpac)/(dfbig+dfpac)
det(covbig)
det(covpac)
det(covpool)

chi_square = (dfbig+dfpac)*log(det(covpool))-dfbig*log(det(covbig))-dfpac*log(det(covpac))
chi_square
q = ncol(Bigten)
dfchi_square = q*(q+1)/2
dfchi_square
p_value=1-pchisq(chi_square,dfchi_square)
p_value

tr(covbig)
tr(covpac)
cov2cor(covbig)
cov2cor(covpac)

#2
grades=data.frame(grades)
male = grades[grades$Gender==0,2:6]
female = grades[grades$Gender==1,2:6]
dfmale = nrow(male)-1
dffemale = nrow(female)-1
Spool = (cov(male)*dfmale+cov(female)*dffemale)/(dfmale+dffemale)
sigma11 = Spool[1:3,1:3]
sigma22 = Spool[4,4]
sigma = Spool[1:4,1:4]
det(sigma11)
sigma22
det(sigma)
chisq = 105*(log(det(sigma11))+log(sigma22)-log(det(sigma)))
chisq
dfchisq = 3*1
dfchisq
p_value=1-pchisq(chisq,dfchisq)
p_value

condS = sigma-(Spool[1:4,5]/Spool[5,5])%*%t(Spool[1:4,5])
cond_sigma11 = condS[1:3,1:3]
cond_sigma22 = condS[4,4]
cond_sigma = condS[1:4,1:4]
det(cond_sigma11)
cond_sigma22
det(cond_sigma)
cond_chisq = (105-1)*(log(det(cond_sigma11))+log(cond_sigma22)-log(det(cond_sigma)))
cond_chisq
dfcon_chisq = 3*1
dfcon_chisq
p_value=1-pchisq(cond_chisq,dfcon_chisq)
p_value
cov2cor(Spool[1:4,1:4])
cov2cor(condS[1:4,1:4])

#3
library(faraway)
data(fat)
Y = cbind(log(fat$neck),log(fat$chest),log(fat$abdom),log(fat$hip),log(fat$thigh))
lm_model= lm(Y~log(fat$weight))
pairs(lm_model$residuals,labels=c('neck','chest','abdom','hip','thigh'))           

sigmahat = cov(lm_model$residuals)
v = 252-5
p=1
q=5
f = factanal(covmat=sigmahat,factors=1,n.obs=v+1)
corr0 = f$loadings%*%t(f$loadings) + diag(f$uniquenesses)
chisq = (v - (2*q+5)/6-2*p/3)*log(det(corr0)/det(f$corr))
chisq
dfchisq = ((q - p)^2 - p - q)/2 
dfchisq
p_value=1-pchisq(chisq,dfchisq)
p_value

f2 = factanal(covmat=sigmahat,factors=2,n.obs=v+1)
f2$uniquenesses
print(f2$loadings,cutoff=0)
p=2
corr0 = f2$loadings%*%t(f2$loadings) + diag(f2$uniquenesses)
chisq = (v - (2*q+5)/6-2*p/3)*log(det(corr0)/det(f2$corr))
chisq
dfchisq = ((q - p)^2 - p - q)/2 
dfchisq
p_value=1-pchisq(chisq,dfchisq)
p_value