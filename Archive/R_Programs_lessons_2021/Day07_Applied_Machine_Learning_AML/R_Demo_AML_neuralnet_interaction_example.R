## R_Demo_AML_neuralnet_interaction_example.R
######################################################
## INSTRUCTOR: Christopher Fariss
## COURSE NAME: Advanced Computational Methods for Social Media and Textual Data (2F)
## University of Essex Summer School 2021
##
## Date: 2021-07-24
##
## Please e-mail me if you find any errors or have and suggestions (either email is fine)
## e-mail: cjf0006@gmail.com
## e-mail: cjfariss@umich.edu
######################################################
## Introduction to tutorial:
##
## Overview of Gradient Decent for a linear model with three parameters for an interaction between variables (x, independent variable, features) in their relationship with an outcome variable (y, dependent variable, target variable).
##
## Back propagation is an algorithm that allows us to generalize the gradient decent algorithm we have already considered.
##
## Back propagation applies the gradient decent algorithm sequentially by first evaluating the error or distance between the target variable and the prediction of the target variable, and then using this information to evaluate each of the intermediate or hidden layers in the neural network for each linear combination of features included in that layer.
##
## Note that := means "is defined as". Notationaly, it represents <- in R. I've used <- below but you may also encounter notation with the := symbol.
##
######################################################
## Estimate a single linear equation:
## y = x1 + x2
##
## we assume some function transforms the features, in this case x1 and x2, into a prediction of y:
## y = f(x1 + x2)
##
## represent the equation with an intercept and weights or coefficients (normally we would denote the weights as beta but here we will call them w):
## y = a + w*x1 + w*x2
##
## add subscripts to denote distinct weights in the equation (this is a perceptron network with ZERO "hidden" layers)
## y = a + w[1]*x1 + w[2]*x2
##
## evaluation:
## (1) select starting values for vectors: a_hat, w_hat
## (2) calculate y_hat based on the values from (1): y_hat <- a_hat + w_hat[1]*x1 + w_hat[2]*x2
## (3) calculate the distance between y_hat and y (this is the error)
## (4) calculate averages for the parameter that weights each variable: delta_parameter <- sum(variable * error)/n using the error calculated from (3)
## (5) using the values from (4) update the parameter estimate: new_estimate <- current_estimate - (learning_rate * average_error)
## (6) repeat (2) - (5) until a loss statistic is minimized
##
######################################################
## Estimate a system of linear equations:
## y = l1 + l2
## l1 = x1 + x2
## l2 = x1 + x2
##
## represent the equation with intercepts, and weights or coefficients (normally we would denote the weights as beta but here we will call them w):
## y = a + w*l1 + w*l2
## l1 = a + w*x1 + w*x2
## l2 = a + w*x1 + w*x2
##
## add subscripts to denote distinct weights in the equation (this is a perceptron network with one "hidden" layer; i.e., l1 and l2)
## y = a[3] + w[3,1]*l1 + w[3,2]*l2
## l1 = a[1] + w[1,1]*x1 + w[1,2]*x2
## l2 = a[2] + w[2,1]*x1 + w[2,2]*x2
##
## evaluation using back propagation:
## (1) select starting values for vectors: a_hat, w_hat
## (2) calculate y_hat based on the values from (1): y_hat <- a_hat[3] + w_hat[3,1]*l1_hat + w_hat[3,2]*l2_hat
##                                                  l1_hat <- a_hat[1] + w_hat[1,1]*x1 + w_hat[1,2]*x2
##                                                  l2_hat <- a_hat[2] + w_hat[2,1]*x1 + w_hat[2,2]*x2
## (3) calculate the distance between y_hat and y (this is the error)
## (4a) calculate averages for the parameter that weights each hidden layer: delta_parameter := sum(hidden_layer * error)/n using the error calculated from (3)
## (4b) calculate averages for the parameter that weights each variable: delta_parameter := sum(variable * error)/n using the error calculated from (4a)
## (5) using the values from (4) update the parameter estimate: new_estimate <- current_estimate - (learning_rate * average_error)
## (6) repeat (2) - (5) until a loss statistic is minimized
##
##
######################################################


## load libraries
library(MASS)
library(neuralnet)

## set learning rate this varies on the unit interval (0 to 1]
lr <- .05

## true generative model
n <- 1000
x1 <- rnorm(n)
x2 <- rnorm(n)

b0 <- -4
b1 <- -2
b2 <- 2
b3 <- .5

y <- b0 + b1*x1 + b2*x2 + b3*x1*x2 + rnorm(n)


## linear fit
fit <- lm(y ~ x1 + x2)
summary(fit)
y_hat <- predict(fit)
sqrt(mean((y-y_hat)^2))

## linear fit with squared term, which is the true model
fit <- lm(y ~ x1 * x2)
summary(fit)
y_hat <- predict(fit)
sqrt(mean((y-y_hat)^2))



## let's pretend we do not know the true model and only have knowledge that y is somehow related to our feature matrix X
X_mat <- cbind(1,x1,x2)

## start with a random guess
iterations <- 1000

## this is the intercept for each of the linear equations, which is often called the bias
alpha_hat <- array(NA, c(3, iterations+1))
alpha_hat[1,1] <- runif(1,-1,1)
alpha_hat[2,1] <- runif(1,-1,1)
alpha_hat[3,1] <- runif(1,-1,1)
dim(alpha_hat)

## these are the betas for each of the equations, which are often called the weights
w_hat <- array(NA, c(6, iterations+1))
w_hat[1,1] <- runif(1,-1,1)
w_hat[2,1] <- runif(1,-1,1)
w_hat[3,1] <- runif(1,-1,1)
w_hat[4,1] <- runif(1,-1,1)
w_hat[5,1] <- runif(1,-1,1)
w_hat[6,1] <- runif(1,-1,1)
dim(w_hat)

## matrices for storing linear transformations
l1 <- l2 <- matrix(NA, nrow=n, ncol=iterations+1)
dim(l1)
dim(l2)

loss <- variance <- NA

y_hat <- y_error <- l2_error <- l1_error <- matrix(NA, nrow=n, ncol=iterations)

delta_alpha <- matrix(NA, nrow=3, ncol=iterations)
delta_w <- matrix(NA, nrow=6, ncol=iterations)

## for loop start
for (j in 1:iterations){
    
    ## calculate the predicted y_hat based on the observed x variable and the best guess of alpha and beta
    
    ## observed layer: feed foward with the first set of linear transformations
    l1[,j] <- alpha_hat[1,j] + w_hat[1,j] * x1 + w_hat[2,j] * x2
    l2[,j] <- alpha_hat[2,j] + w_hat[3,j] * x1 + w_hat[4,j] * x2
    
    ## hidden layer: second linear transformation
    y_hat[,j] <- alpha_hat[3,1] + w_hat[5,1]*l1[,j] + w_hat[6,1]*l2[,j]
    
    ## difference between the predicted y (y_hat) and y is the error for y
    y_error[,j] <- y_hat[,j] - y
    l1_error[,j] <- (y_hat[,j] - y) * w_hat[5,1]
    l2_error[,j] <- (y_hat[,j] - y) * w_hat[6,1]
    
    ## the estimated error is used to calculate the unexplained variance between y and y_hat, which is the sum of squared errors
    loss[j] <- sqrt(mean((y-y_hat[,j])^2))
    
    ## calculate the gradient at that point (this works because the errors are independent to the column vectors in X
    delta_alpha[3,j] <- sum(X_mat[,1] * y_error[,j])/n
    delta_alpha[1,j] <- sum(X_mat[,1] * l1_error[,j])/n
    delta_alpha[2,j] <- sum(X_mat[,1] * l2_error[,j])/n
    
    delta_w[5,j] <- sum(l1[,j] * y_error[,j])/n
    delta_w[6,j] <- sum(l2[,j] * y_error[,j])/n
    delta_w[1,j] <- sum(X_mat[,2] * l1_error[,j])/n
    delta_w[2,j] <- sum(X_mat[,3] * l1_error[,j])/n
    delta_w[3,j] <- sum(X_mat[,2] * l2_error[,j])/n
    delta_w[4,j] <- sum(X_mat[,3] * l2_error[,j])/n
    
    ## shift estimates along the gradient (+ function if y-y_hat; - function if y_hat-y)
    alpha_hat[1,j+1] <- alpha_hat[1,j] - (lr*delta_alpha[1,j])
    alpha_hat[2,j+1] <- alpha_hat[2,j] - (lr*delta_alpha[2,j])
    alpha_hat[3,j+1] <- alpha_hat[3,j] - (lr*delta_alpha[3,j])
    
    w_hat[1,j+1] <- w_hat[1,j] - (lr*delta_w[1,j])
    w_hat[2,j+1] <- w_hat[2,j] - (lr*delta_w[2,j])
    w_hat[3,j+1] <- w_hat[3,j] - (lr*delta_w[3,j])
    w_hat[4,j+1] <- w_hat[4,j] - (lr*delta_w[4,j])
    w_hat[5,j+1] <- w_hat[5,j] - (lr*delta_w[5,j])
    w_hat[6,j+1] <- w_hat[6,j] - (lr*delta_w[6,j])
}


alpha_hat[,1000]

w_hat[,1000]



loss

## evaulation 
plot(loss)

loss[1000]

sqrt(mean((y-y_hat[,1000])^2))


## load a library for estimating the same nueralnet model
library(neuralnet)

test <- data.frame(y,x1, x2)

fit <- lm(y ~ x1 * x2, data=test)
summary(fit)
rmse.fit <- sqrt(mean((y - predict(fit))^2))
rmse.fit
fit$coefficients

nn <- neuralnet(y ~ x1 + x2, data=test, hidden=c(0), linear.output=T, err.fct="sse")
plot(nn)
nn_hat <- unlist(nn$net.result)
rmse.nn <- sqrt(mean((y - nn_hat)^2))
rmse.nn
nn$result.matrix

attributes(nn)

nn <- neuralnet(y ~ x1 + x2, data=test, hidden=c(2), linear.output=T, err.fct="sse")
plot(nn)
#pr.nn <- compute(nn, covariate=data[,2])
nn_hat <- unlist(nn$net.result)
rmse.nn <- sqrt(mean((y - nn_hat)^2))
rmse.nn
nn$result.matrix

nn <- neuralnet(y ~ x1 + x2, data=test, hidden=c(4), linear.output=T, err.fct="sse")
plot(nn)
#pr.nn <- compute(nn, covariate=data[,2])
nn_hat <- unlist(nn$net.result)
rmse.nn <- sqrt(mean((y - nn_hat)^2))
rmse.nn
nn$result.matrix

nn <- neuralnet(y ~ x1 + x2, data=test, hidden=c(1,1), linear.output=T, err.fct="sse")
plot(nn)
#pr.nn <- compute(nn, covariate=data[,2])
nn_hat <- unlist(nn$net.result)
rmse.nn <- sqrt(mean((y - nn_hat)^2))
rmse.nn
nn$result.matrix


fit <- lm(y ~ x1 * x2, data=test)
summary(fit)
rmse.fit <- sqrt(mean((y - predict(fit))^2))
rmse.fit
fit$coefficients









