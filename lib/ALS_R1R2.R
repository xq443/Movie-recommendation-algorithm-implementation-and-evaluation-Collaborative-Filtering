### Alternating Least Square (R1+R2)
### Author: Mengying Shi

#Define a function to calculate RMSE
RMSE <- function(rating, est_rating){
  sqr_err <- function(obs){
    sqr_error <- (obs[3] - est_rating[as.character(obs[1]), as.character(obs[2])])^2
    return(sqr_error)
  }
  return(sqrt(mean(apply(rating, 1, sqr_err))))  
}


# Alternating least squares (R1+R2)
# a function returns a list containing factorized matrices p and q, training and testing RMSEs.

ALS_R1R2 <- function(f = 10,lambda = 0.3,max.iter,data,train, test){
  # Step 1: Initialize Movie matrix (q), User matrix (p), Movie bias(bi) and User bias(bu)
  p <- matrix(runif(f*U, -1, 1), ncol = U) 
  colnames(p) <- as.character(1:U)
  q <- matrix(runif(f*I, -1, 1), ncol = I)
  colnames(q) <- levels(as.factor(data$movieId))
  bi <- matrix(rep(0, I), ncol = I)
  colnames(bi) <- levels(as.factor(data$movieId))
  bu <- matrix(rep(0, U), ncol=U)
  colnames(bu) <- levels(as.factor(data$userId))
  
  train_RMSE <- c()
  test_RMSE <- c()
  mu <- mean(train$rating)
  
  movie.id <- sort(unique(data$movieId))

  for (l in 1:max.iter){
    p_intercept <- rbind(bu, p)
    q_intercept <- rbind(rep(1,I), q)
    colnames(q_intercept) <- levels(as.factor(data$movieId))
    
    for (u in 1:U) {
      j <- as.character(train[train$userId==u,]$movieId) # the moives rated by user u
      q_j <- q_intercept[,j] 
      # update p_intercept
      p_intercept[,u] <- solve(q_j %*% t(q_j) + lambda * diag(f+1)) %*%
                          q_intercept[,j] %*% 
                          (train[train$userId==u,]$rating - mu -bi[,j])
    }
    # update bu and p
    bu[1,] <- p_intercept[1, ]
    p <- p_intercept[-1, ]
    
    for (m in 1:I) {
      i <- as.character(train[train$movieId==movie.id[m],]$userId) # find the users who rate movie m
      p_i <- p_intercept[,i]
      # update q_intercept
      q_intercept[,m] <- solve(p_i %*% t(p_i) + lambda* diag(f+1)) %*%
                          p_intercept[,i] %*% 
                          (train[train$movieId==movie.id[m],]$rating - mu - bu[,i])
      
    }
    # update bi and q
    bi[1,] <- q_intercept[1,]
    q <- q_intercept[-1,]
    
    #print the values of training and testing RMSE
    cat("iter:", l, "\t")
    est_rating <- t(p) %*% q + mu + bu[1,] + rep(bi[1,], each = ncol(p))
    colnames(est_rating) <- levels(as.factor(data$movieId))
    
    train_RMSE_cur <- RMSE(train, est_rating)
    cat("training RMSE:", train_RMSE_cur, "\t")
    train_RMSE <- c(train_RMSE, train_RMSE_cur)
    
    test_RMSE_cur <- RMSE(test, est_rating)
    cat("test RMSE:",test_RMSE_cur, "\n")
    test_RMSE <- c(test_RMSE, test_RMSE_cur)
  
  }
  return(list(p = p, q = q, bu = bu, bi = bi, mu= mu, train_RMSE = train_RMSE, test_RMSE = test_RMSE, ALS.rating=est_rating))
}


