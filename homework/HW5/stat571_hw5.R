first = rep(1,100)
second = c(1:100)
cubic = matrix(c(first,second,second^2,second^3),nrow = 100, ncol = 4)
R = qr.R(qr(cubic))
Q = qr.Q(qr(cubic))
matplot(Q[1:100,1],type='l',
        ylab='first column of Q',xlab='time points',main='constant')
matplot(Q[1:100,2],type='l',
        ylab='second column of Q',xlab='time points',main='linear')
matplot(Q[1:100,3],type='l',
        ylab='third column of Q',xlab='time points',main='quadratic')
matplot(Q[1:100,4],type='l',
        ylab='forth column of Q',xlab='time points',main='cubic')
