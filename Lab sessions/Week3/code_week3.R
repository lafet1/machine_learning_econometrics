# Lab session 3
library(tidyverse)
library(profvis)

# function for data generation
myFunc <- function(n){
  x <- runif(n, min=-1, max=1)
  error <- rnorm(n, mean=0, sd=0.2)
  target <- (-0.3+0.5*x)+error
  df <- data.frame(target,x)
  return(df)
}

set.seed(12345678)
myValues <- myFunc(1000)
ggplot(myValues, aes(x=x, y=target)) + geom_point(shape=1)

# specifying OLS
olsSolution <- function(mydata){
  mydata <- cbind(intercept = rep(1, nrow(mydata)), mydata)
  train_data <- unname(as.matrix(mydata[, -2])) 
  w_ols <- (solve(t(train_data) %*% train_data) %*% t(train_data)) %*% mydata[[2]]
  predic_values <- (w_ols[2, 1] * myValues[[2]]) + w_ols[1, 1]
  resid_values <-  (predic_values-myValues[[1]])
  return(list = c(w_0 = w_ols[1, 1], w_1 = w_ols[2, 1], sd_res = sd(resid_values)))
}

lm_ols <- olsSolution(myValues)
print(lm_ols)

# again OLS by lm in R
rSolution <- function(mydata){
  reg_model <- lm(target ~ x, data = mydata)
  return(list = c(w_0 = unname(coefficients(reg_model)[1]), 
                  w_1 = unname(coefficients(reg_model)[2]), 
                  sd_res = sd(residuals(reg_model))))
}

lm_r <- rSolution(myValues)
print(lm_r)

# doing this by ML
likelifunc <- function(estima,mydata){
  
  n <- nrow(mydata)
  w_0 <- estima[1]
  w_1 <- estima[2]
  beta_est <- estima[3]
  coef <- c(w_0, w_1)
  x <- unname(as.matrix(mydata[, -2]))
  target <- mydata$target
  
  res <- target - (x %*% coef)
  logl <- n * 0.5 * log(beta_est) - n * 0.5 * log(2 * pi) - 
    beta_est * (sum(res^2))
  return(-logl)
  
}

mlSolution <- function(mydata){
  mydata <- cbind(intercept = rep(1, nrow(mydata)), mydata)
  
  p <- optim(c(1, 1, 1), likelifunc, method = "BFGS", hessian = T, mydata = mydata)
  return(list = c(w_0 = p$par[1], w_1 = p$par[2], sd_res = sqrt(1 / p$par[3])))
  
  
}

lm_ml <- mlSolution(myValues)
print(lm_ml)

# doing it by gradient descent
sgdSolution <- function(estima, mydata, learning_rate){
  new_w0 <- estima[1]
  new_w1 <- estima[2]
  
  grad_w0 <- function(obsx, predx){
    return(-(obsx - predx))
  }
  
  grad_w1 <- function(obsx, predx, inputx){
    return(-(inputx) * (obsx - predx))
  }
  
  for(obs in 1:nrow(mydata)){
    pred_val <- sum(c(new_w0,new_w1) * c(1, mydata$x[obs]))
    new_w0 <- new_w0 - learning_rate * grad_w0(mydata$target[obs], pred_val)
    new_w1 <- new_w1 - learning_rate * grad_w1(mydata$target[obs], pred_val, 
                                               inputx = mydata$x[obs])
  }
  
  return(list = c(w_0 = new_w0, w_1 = new_w1))
  
}

lm_sgd <- sgdSolution(estima = c(1, 1), mydata = myValues, learning_rate = 0.01)
print(lm_sgd)

# regularized OLS
olsregSolution <- function(mydata, lambda){
  mydata <- cbind(intercept = rep(1, nrow(mydata)), mydata)
  train_data <- unname(as.matrix(mydata[, -2])) 
  w_ols_reg <- (solve(lambda * diag(2) + t(train_data) %*% train_data) %*% 
                  t(train_data)) %*% mydata[[2]]
  return(list(w_0 = w_ols_reg[1, 1], w_1 = w_ols_reg[2, 1]))
}


lmregular <- olsregSolution(mydata = myValues, lambda = 0.1)
print(lmregular)

# bayes regression
bayesSolution <- function(mydata, alpha, beta){
  mydata <- unname(as.matrix(cbind(intercept = rep(1, nrow(mydata)), mydata)))
  alphaI <- alpha * diag(1, 2)
  for(obs in 2:nrow(mydata)){
    S <- solve(alphaI + beta * t(mydata[1:obs, -2]) %*% mydata[1:obs, -2])
    m <- as.vector(beta * S %*%  t(mydata[1:obs, -2]) %*% matrix(mydata[1:obs, 2]))
  }
  
  return(list(w_0 = m[1], w_1 = m[2]))
}

lm_br <- bayesSolution(mydata = myValues, alpha = 2, beta = 25)
print(lm_br)

# comparison
compare_df_w0 <- data.frame(num_point = numeric(), ols = numeric(),
                            ml = numeric(), sgd = numeric(),
                            regular = numeric(),br = numeric())
compare_df_w1 <- data.frame(num_point = numeric(), ols = numeric(),
                            ml = numeric(), sgd = numeric(),
                            regular = numeric(),br = numeric())

for(k in seq(50, 1000, 50)){
  lm_ols <- olsSolution(myValues[1:k,])
  lm_ml <- mlSolution(myValues[1:k,])
  lm_sgd <- sgdSolution(estima = c(1, 1), mydata = myValues[1:k,], 
                        learning_rate = 0.01)
  lmregular <- olsregSolution(mydata = myValues[1:k,], lambda = 0.1)
  lm_br <- bayesSolution(mydata = myValues[1:k,], alpha = 2, beta = 25)
  
  compare_df_w0 <- rbind(compare_df_w0, data.frame(num_point = k,ols = lm_ols[1], 
                                                  ml = lm_ml[1], sgd = lm_sgd[1],
                                                  regular = lmregular$w_0,
                                                  br = lm_br$w_0), make.row.names = F)
  compare_df_w1 <- rbind(compare_df_w1,data.frame(num_point = k,ols = lm_ols[2], 
                                                  ml = lm_ml[2],sgd = lm_sgd[2],
                                                  regular = lmregular$w_1,
                                                  br = lm_br$w_1), make.row.names=F)
  
}

head(compare_df_w1)








