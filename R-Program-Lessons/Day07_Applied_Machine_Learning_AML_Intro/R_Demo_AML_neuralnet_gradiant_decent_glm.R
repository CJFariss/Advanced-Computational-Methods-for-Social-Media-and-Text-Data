## R_Demo_AML_neuralnet_gradiant_decent_glm.R
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
## Overview of Gradient Decent for generalized linear model example.
##
## Gradient decent is an iterative process used to find the best parameter value(s) in any model and especially a neural network.
##
##########################################################################

## set learning rate this varies on the unit interval (0 to 1]
library(MASS)
library(boot)
library(gtools)

lr <- .5
n <- 100
x <- rnorm(n)

alpha <- -1
beta <- -2

y <- rbinom(1:n, 1, prob=inv.logit(alpha + beta*x))
#y <- rbinom(1:n, 1, prob=inv.logit(alpha + beta*x + rnorm(n)))

## Start with a random guess
X_mat <- cbind(1,x)

alpha_hat <- 4
beta_hat <- 4
iterations <- 1000

loss <- variance <- NA

y_hat <- matrix(NA, nrow=n, ncol=iterations)
y_error <- matrix(NA, nrow=n, ncol=iterations)
delta <- matrix(NA, nrow=2, ncol=iterations)

## sequential iterations to evaluate loss function
for (j in 1:iterations){
    
    ## calculate the predicted y_hat based on the observed x variable and the best guess of alpha and beta
    y_hat[,j] <- inv.logit(alpha_hat[j] + beta_hat[j] * x)

    ## this works too but not as well as above
    #y_hat[,j] <- rbinom(1:n,1,prob=inv.logit(alpha_hat[j] + beta_hat[j] * x))
    
    ## difference between the predicted y (y_hat) and y is the error for y
    y_error[,j] <- y_hat[,j] - y
    
    ## the estimated error is used to calculate the unexplained variance between y and y_hat, which is the sum of squared errors
    loss[j] <- sum(y_error[,j]^2)
    
    ## calculate the gradient at that point (this works because the errors are independent to the column vectors in X
    ##delta[1:2,j] <- (t(X_mat) %*% y_error[,j]) * (1/n)
    delta[1,j] <- sum(X_mat[,1] * y_error[,j])/n
    delta[2,j] <- sum(X_mat[,2] * y_error[,j])/n
    
    ## shift estimates along the gradient (+ function if y-y_hat; - function if y_hat-y)
    alpha_hat[j+1] <- alpha_hat[j] - (lr*delta[1,j])
    beta_hat[j+1] <- beta_hat[j] - (lr*delta[2,j])
}

## estimate glm model
fit <- glm(y~x, family=binomial("logit"))
summary(fit)

## print the last value of the sequence of parameter estimates
alpha_hat[length(alpha_hat)]
beta_hat[length(beta_hat)]


## graph the values as a function of the loss statistic
ALPHA_seq <- seq(from=-4,4,.05)
BETA_seq <- seq(from=-4,4,.05)

LOSS <- matrix(NA, nrow=length(ALPHA_seq), ncol=length(BETA_seq))

## grid search for graphing
for(i in 1:length(ALPHA_seq)){
    for(j in 1:length(ALPHA_seq)){
        Y_hat <- inv.logit(ALPHA_seq[i] + BETA_seq[j] * X_mat[,2])
        LOSS[i,j] <- sum((y-Y_hat)^2)
        
    }
}

## plot the log loss
contour(ALPHA_seq, BETA_seq,(LOSS), xlab=expression(hat(alpha)), ylab=expression(hat(beta)), cex.lab=1.5)
lines(alpha_hat, beta_hat, col=2, lwd=4)
points(alpha_hat, beta_hat, col="navy", pch=.5)

