library(glmnet)
set.seed(1)

v <- c(rep(-1,500),rep(1,500))
xmat <- replicate(10000,sample(v,replace=TRUE))
x <- rowSums(xmat) 

y <- NULL
for(i in seq(1000)){
  y <- c(y,x[i] + rnorm(1,mean=0,sd=1))
}

data <- data.frame(cbind(as.matrix(y),xmat))

data <- data[sample(nrow(data)),]
fold <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE)
grid <- c(0.01,0.02,0.05,0.1,0.2,0.5,1,2,5,10,20,50,100)

test <- NULL; tran <- NULL; risk <- NULL
num_folds
for(i in seq(10)){
  testIndices <- which(fold==i,arr.ind=TRUE)
  ridge.mod <- glmnet(xmat[-testIndices,],y[-testIndices],alpha=0,lambda=grid,intercept=FALSE)
  for(j in seq(13)){
    ridge.pred <- predict(ridge.mod,s=grid[j],newx=xmat[testIndices,])
    test <- c(test,mean((ridge.pred-y[testIndices])^2))
    
    ridge.pred_tran <- predict(ridge.mod,s=grid[j],newx=xmat[-testIndices,])
    tran <- c(tran,mean((ridge.pred_tran-y[-testIndices])^2))
    
    ridge.pred_all <- predict(ridge.mod,s=grid[j],newx=xmat)
    risk <- c(risk,mean((ridge.pred_all-y)^2))
  }
}