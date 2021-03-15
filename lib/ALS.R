#Define a function to calculate RMSE
RMSE <- function(rating, est_rating){
  sqr_err <- function(obs){
    sqr_error <- (obs[3] - est_rating[as.character(obs[1]), as.character(obs[2])])^2
    return(sqr_error)
  }
  return(sqrt(mean(apply(rating, 1, sqr_err))))  
}



ALS <- function(f = 10, lambda = 0.3, max.iter=20, data, train=data_train, test=data_test) {
  
  # Initialize Movie Matrix and User Matrix
  Movie <- matrix(runif(f*I, -1, 1), ncol = I)
  colnames(Movie) <- levels(as.factor(data$movieId))
  movie.average <- data %>% group_by(movieId) %>% summarize(ave=mean(rating))
  Movie[1,] <- movie.average$ave
  
  
  User <- matrix(runif(f*U, -1, 1), ncol = U) 
  colnames(User) <- levels(as.factor(data$userId))
  
  
  movie.id <- sort(unique(data$movieId))
  train_RMSE <- c()
  test_RMSE <- c()
  
  for (l in 1:max.iter){
  
  # Step2: Fix M, Solve U
  for (u in 1:U) {
    
    User[,u] <- solve (Movie[,as.character(train[train$userId==u,]$movieId)] %*%
      t(Movie[,as.character(train[train$userId==u,]$movieId)]) + lambda * diag(f)) %*%
      Movie[,as.character(train[train$userId==u,]$movieId)] %*% train[train$userId==u,]$rating}
    
    
  # Step3: Fix U, Solve M  
  for (i in 1:I) {
    Movie[,i] <- solve (User[,train[train$movieId==movie.id[i],]$userId] %*% 
      t(User[,train[train$movieId==movie.id[i],]$userId]) + lambda * diag(f)) %*%
      User[,train[train$movieId==movie.id[i],]$userId] %*% train[train$movieId==movie.id[i],]$rating
    
  }
    
    
    # Summerize
    cat("iter:", l, "\t")
    est_rating <- t(Movie) %*% User 
    colnames(est_rating) <- levels(as.factor(data$movieId))
    
    train_RMSE_cur <- RMSE(train, est_rating)
    cat("training RMSE:", train_RMSE_cur, "\t")
    train_RMSE <- c(train_RMSE, train_RMSE_cur)
    
    test_RMSE_cur <- RMSE(test, est_rating)
    cat("test RMSE:",test_RMSE_cur, "\n")
    test_RMSE <- c(test_RMSE, test_RMSE_cur)
    
  } 
}
