### Cross validation in KRR
cv.krr <- function(data_train, 
                   kfold, 
                   lambda){
  # Input:
  # tuning parameter: lambda
  # The number of folds: K-folds
  # training data set
  
  ### Split data into predictors and response
  x <- as.matrix(data_train[,-1])
  y <- data_train[,1]
  n <- dim(x)[1]
  n.fold <- round(n/kfold,0)
  set.seed(0)
  id.cv <- createFolds(1:n, k = kfold)
  tuning.cv <- c()
  # folds <-sample(rep(1:kfold,c(rep(n.fold,kfold - 1), n - (kfold - 1) * n.fold)))
  # train.cv.error <- rep(NA, kfold)
  # test.cv.error <- rep(NA, kfold)
  for (i in id.cv){
    #Split data into training and validation data sets
    train.x <- x[-i, ]
    train.y <- y[-i]
    valid.x <- x[i,]
    valid.y <- y[i]
    model.cv <- krr(x = train.x, train.y, lambda = lambda)
    pred.cv <- predict(model.cv, valid.x)
    rmse.cv <- sqrt(mean((valid.y - pred.cv)^2))
    tuning.cv <- cbind(tuning.cv, rmse.cv)
    mean.cv <- mean(tuning.cv)
  }
  return(mean.cv)
}