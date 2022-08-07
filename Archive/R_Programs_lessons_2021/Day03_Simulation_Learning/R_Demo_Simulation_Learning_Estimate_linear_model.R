## R_Demo_Simulation_Learning_Estimate_linear_model.R
##########################################################################
## INSTRUCTOR: Christopher Fariss
## COURSE NAME: Advanced Computational Methods for Social Media and Textual Data (2F)
## University of Essex Summer School 2021
##
## Date: 2021-07-24
##
## Please e-mail me if you find any errors or have and suggestions (either email is fine)
## e-mail: cjf0006@gmail.com
## e-mail: cjfariss@umich.edu
##########################################################################
##
## Introduction to tutorial:
##
## (1) simulates two variable OLS regression in R using:
## (1a) a brute force method (grid search)
## (1b) the lm function
## (1c) and a user defined function which is passed to the optim function
## (2) the brute force method is used to visualize the parameter space using the contour plot function from the graphics library
##
##########################################################################

## install the library if necessary
## install.packages(c("graphics", "mvtnorm"))

## load libraries
library(graphics)
library(mvtnorm)

## generate simulated vales
n <- 100
x1 <- rnorm(n,0,1)
X <- cbind(1, x1)

k <- ncol(X)

## select true population parameters
alpha <- 1.250000
beta <- 2.500000

## generate y with error
y <- alpha * X[,1] + beta * X[,2] + rnorm(n)

y <- alpha  + beta * x1 + rnorm(n)


## generate vector of possible values for the parameter estimates of alpha and beta
alpha.hat <- seq(from=-4,6,.05)
beta.hat <- seq(from=-4,6,.05)

sumsquare <- matrix(NA, nrow=length(alpha.hat), ncol=length(beta.hat))

for(i in 1:length(alpha.hat)){
    for(j in 1:length(beta.hat)){
        y.hat <- alpha.hat[i] + beta.hat[j] * X[,2]
        sumsquare[i,j] <- -sum((y-y.hat)^2)
    }
}

## find the coordinates from the matrix where the minimum of the sum of square residulas resides
coordinates <- which(sumsquare == sumsquare[-sumsquare==min(-sumsquare)], arr.ind = TRUE)
coordinates

## these coordinates match the best estimates of the alpha and beta parameters
par <- c(alpha.hat[coordinates[1]], beta.hat[coordinates[2]])
par ## notice that these estimates are much less precise than the estiamtes obtained below

## counter plot from the brute force method (logged for visualization)
par(mar=c(5,5,1,1))
contour(alpha.hat,beta.hat,log(-sumsquare), xlab=expression(hat(alpha)), ylab=expression(hat(beta)), cex.lab=1.5)

## generate variables for monitoring the function
eval <- array(dim=c(1000,3))
iter <- 1

# user defined function passed to optim
ols.func <- function(par, X, iterate=TRUE){
    alpha.hat <- par[1]
    beta.hat <- par[2]
    y.hat <- alpha.hat + beta.hat * X[,2]
    out <- -sum((y-y.hat)^2)
    
    if(iterate==TRUE){
        eval[iter,1] <<- alpha.hat
        eval[iter,2] <<- beta.hat
        eval[iter,3] <<- out
        iter <<- iter+1
    }
    return(out)
}

## pass function to optim with initial values
optim.out <- optim(par = c(0,0), ols.func, X=X, method="BFGS", control=list(fnscale = -1), hessian = TRUE)
optim.out

se <- sqrt(diag(solve(-optim.out$hessian))) #calculate standard errors
VCV <- solve(-optim.out$hessian) #compute variance-covariance matrix
se
VCV

## compare with results from the lm function
fit <- lm(y~x1)
summary(fit)


## plot gradient paths for the following algorithims: "Nelder-Mead", "BGFS", "CG", "L-BFGS-B", "SANN"
eval <- array(dim=c(1500,4))
iter <- 1
optim.out <- optim(par = c(0,0), ols.func, X=X, method="Nelder-Mead", control=list(fnscale = -1), hessian = TRUE)
contour(alpha.hat,beta.hat,log(-sumsquare), xlab=expression(hat(alpha)), ylab=expression(hat(beta)), cex.lab=1.5)
lines(eval[1:iter,1], eval[1:iter,2], col=1, lwd=4)
optim.out$par
iter

eval <- array(dim=c(1500,4))
iter <- 1
optim.out <- optim(par = c(0,0), ols.func, X=X, method="BFGS", control=list(fnscale = -1), hessian = TRUE)
lines(eval[1:iter,1], eval[1:iter,2], col=2, lwd=4)
optim.out$par
iter

eval <- array(dim=c(1500,4))
iter <- 1
optim.out <- optim(par = c(0,0), ols.func, X=X, method="CG", control=list(fnscale = -1), hessian = TRUE)
lines(eval[1:iter,1], eval[1:iter,2], col=3, lwd=4)
optim.out$par
iter

eval <- array(dim=c(1500,4))
iter <- 1
optim.out <- optim(par = c(0,0), ols.func, X=X, method="L-BFGS-B", control=list(fnscale = -1), hessian = TRUE)
lines(eval[1:iter,1], eval[1:iter,2], col=4, lwd=4)
optim.out$par
iter

eval <- array(dim=c(20000,4))
iter <- 1
optim.out <- optim(par = c(0,0), ols.func, X=X, method="SANN", control=list(fnscale = -1), hessian = TRUE)
lines(eval[1:iter,1], eval[1:iter,2], col=5, lwd=4)
optim.out$par
iter



