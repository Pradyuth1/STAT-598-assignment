#Name : Pradyuth Vangur

#Problem 1: Function for generating training dataset
gen_data <- function(n, p, sparsity, level){
  X <- matrix(rnorm(n*p, mean = 0, sd = 1), nrow = n, ncol = p)
  w <- matrix(rep(0, p), nrow = p, ncol = 1)
  w[1:sparsity,] <- level
  Y <- X%*%w
  return(list(X,Y))
}

#Problem 2: Creating loss function
lasso_loss <- function(w, lambda){
  func_to_optim <- (sum(((y_glbl - (x_glbl %*% w))^2)) + lambda*(sum(abs(w))))
  return(func_to_optim)
}

#Problem 3: Generating training dataset
A <- gen_data(50, 100, 5, 5)
x_glbl <- A[[1]]
y_glbl <- A[[2]]
w_glbl <- matrix(rep(0, 100), nrow = 100, ncol = 1)
w_glbl[1:5,] <- 5

#Problem 4: Using the optim function to find best values of w
w_values <- optim(par = matrix(rep(1,100)),fn = lasso_loss, lambda = 1)
w_values
plot(w_glbl, type = "p", col = "red", ylab = "W values",
     main = "True values in red vs optimised values in blue")
points(w_values$par, col = "blue")

#Problem 5: Using the optim function to find the best values of w and lambda
input <- function(ip_1){
  a <- lasso_loss(ip_1[-1], ip_1[1])
  return(a)
}

w_values_1 <- optim(par = c(1, matrix(rep(0,100))),fn = input)

plot(w_glbl, type = "p", col = "red", ylab = "W values",
     main = "True values in red vs optimised values in blue")
points(w_values_1$par[-1], col = "blue")

#Coordinate descent
#Problem 1: Writing soft threshold function
soft_threshold <- function(w, th){
  value <- ifelse((abs(w) < th), 0, sign(w)*(abs(w) - th))
  return(value)
}

plot(soft_threshold(-6:6, 1), type = "b", col = "red", ylab = "Threshold values")


#Problem 2: Lasso1d function
lasso1d <- function(x, y, lambda){
  w <- ((t(y) %*% x)/ (t(x) %*% x))
  th <- (lambda/ (t(x)%*%x))
  return(soft_threshold(w, th))
}



#Problem 3: Capturing the residuals 
get_residual <- function(w, dim, X, Y){
  w[dim] <- 0
  Y_pred <- X %*% w
  Y_res <- (Y - Y_pred)
  return(Y_res)
}


#Problem 4: 
#value stands for the estimated percentage change
cor_desc <- function(w, lambda, value, X, Y){
  w_new <- w*(value + 1)
  w_old <- w
  while(abs(((w_new - w_old)*100)) >= abs(w_old)*value){
    w_old <- w_new
    for(i in 1:length(w)){
      x_ele <- X[,i]
      residual <- get_residual(w_new, i, X, Y)
      w_calc <- ((t(x_ele) %*% residual) / (t(x_ele) %*% x_ele))
      th <- (lambda/ (t(x_ele)%*%x_ele))
      w_new[i] <- soft_threshold(w_calc, th)
    }
  }
  return(w_new)
}


#Problem 5:
cor_desc(rep(0,100), 1, 2, x_glbl, y_glbl)
#We observe that the values obtained from the cor_desc is 
#way better than optim function values

#Problem 6:
L2 <- list(rep(0,10))
j <- 1
for(i in seq(0,45,5)){
  L2[[j]] <- cor_desc(rep(0,100), 1, 50, x_glbl[(i+1):(i+5), ], y_glbl[(i+1):(i+5), ])
  j <- j + 1
}


L2_error <- rep(0,10)
for(i in 1:10){
  L2_error[i] <- sqrt(abs(sum(L2[[i]]^2 - w_glbl^2)))
}

plot(seq(0,45,5), L2_error, xlab = seq(0,45,5), ylab = "L2_error", type = "b", 
     main = "L2 error", col = "blue", xlim = c(1, 50))

