## R_Demo_AML_neuralnet_gradiant_decent_mu.R
##########################################################################
## INSTRUCTOR: Christopher Fariss
## COURSE NAME: Advanced Computational Methods for Social Media and Textual Data (3B)
## University of Essex Summer School 2023
##
## Date: 2023-08-13
##
## Please e-mail me if you find any errors or have and suggestions (either email is fine)
## e-mail: cjf0006@gmail.com
## e-mail: cjfariss@umich.edu
##########################################################################
## Introduction to tutorial:
##
## Overview of Gradient Decent for a model with one parameter for a mean.
##
## Gradient decent is an iterative process used to find the best parameter value(s) in any model and especially a neural network.
##
##########################################################################





## load libraries
library(MASS)

## set learning rate this varies on the unit interval (0 to 1]
lr <- .2
lr <- .05
lr <- .9

y <- c(1,2,3,4,5)
##y <- rnorm(1000, pi, 1)
n <- length(y)

y
n


iterations <- 100

loss <- NA

mu_hat <- -1.5

y_hat <- matrix(NA, nrow=n, ncol=iterations)
y_error <- matrix(NA, nrow=n, ncol=iterations)

delta_mu_hat <- NA

## sequential iterations to evaluate loss function
for (j in 1:iterations){
    
    ## y_hat: calculate the predicted y_hat based on the best guess of mu
    
    y_hat[,j] <- mu_hat[j]
    
    ## residual: the difference between the predicted y (y_hat) and y is the error for y
    y_error[,j] <- y_hat[,j] - y
    
    ## loss: the estimated error is used to calculate the unexplained variance between y and y_hat, which is the sum of squared errors
    loss[j] <- sum(y_error[,j]^2)
    
    ## difference between current estimate and the unexplained differences, which is the method to calculate the gradient at that point
    delta_mu_hat[j] <- sum(y_error[,j])/n
    
    ## shift estimates along the gradient (+ function if y-y_hat; - function if y_hat-y)
    mu_hat[j+1] <- mu_hat[j] - (lr*delta_mu_hat[j])
}

mu_hat[length(mu_hat)]

plot(mu_hat)

plot(mu_hat[-1], delta_mu_hat)
