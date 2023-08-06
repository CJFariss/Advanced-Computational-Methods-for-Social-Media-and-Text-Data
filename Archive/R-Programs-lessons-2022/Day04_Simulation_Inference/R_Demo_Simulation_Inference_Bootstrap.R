## R_Demo_Simulation_Inference_Bootstrap
##########################################################################
## INSTRUCTOR: Christopher Fariss
## COURSE NAME: Advanced Computational Methods for Social Media and Textual Data (2F)
## University of Essex Summer School 2022
##
## Date: 2022-08-28
##
## Please e-mail me if you find any errors or have and suggestions (either email is fine)
## e-mail: cjf0006@gmail.com
## e-mail: cjfariss@umich.edu
##########################################################################
##
## Goal: Improve the estimation of uncertainty assoicated with one or more model parameters
##
##########################################################################
## Introduction to tutorial:
##
## We have a sample of n elements: X={x_1,x_2,...,x_n}. 
##
## And we are interested in standard error or confidence interval for a statistic using that data s(X).
##
## s() is any statistics: e.g., mean(), median(), sum(), etc.
##
## The bootstrap repeats R times: 
## 
## For every iteration in R = {1, ... r}, sample with replacement n elements from the available data in the sample (some of the data will be picked more than once).
##
## Calculate the statistic s(X[r]) for all of the R samples.
##
## Calculate the standard deviation of all statistics s(X[r]) calculated from the R samples.
##
## The bootstrap standard error is the standard deviation for sd(s(X[r])).
##
## A bootstrapped standard error can be calculated for any statistic or set of parameters.
##
##########################################################################
##
## a simulation example:
##
## (1) generate a random normal variable of mean=pi, standard deviation=1
##
## (2) use the sample() function 2000 times (with replacement) to take draws of the simulated variable (sometimes the element will be sampled 0 times, sometimes it will not be sampled at all)
##
## (3) calculate the mean and standard deviation of the 2000 samples
##
## (4) repeat this procedure using the boot package
##
## (5) calculate the median and standard deviation of the median statistic 2000 samples
##
## (6) repeat this procedure using the boot package
##
##########################################################################

library(boot)

sim_n <- 30
x <- rnorm(sim_n, mean=pi, sd=1)
x

summary(x)


samples_r <- 2000
x_r <- c()
for(i in 1:samples_r){
    temp <- x[sample(1:length(x),size=sim_n,replace=TRUE)]
    x_r[i] <- mean(temp)
}
mean(x_r)
sd(x_r)

x_r <- lapply(1:samples_r, function(i){
    x_r <- x[sample(1:length(x),size=sim_n,replace=TRUE)]
    return(mean(x_r))
})

## original estimate
mean(x)

## standard error of the mean
mean_se <- function(x){
    sd(x)/sqrt(length(x))
}
mean_se(x)

## bootstrap estimate
mean(unlist(x_r))

## standard error, which is just the standard deviation of all the bootstrap samples
sd(unlist(x_r))

## bias, which is just the difference between the observed statistic and the bootstrapped statistic
mean(unlist(x_r)) - mean(x)

## graph the distribution
truehist(unlist(x_r))


## load the boot package
library(boot)

## define a function for the statistic, which includes an index value, which the boot function needs to perform the sampling
mean_index <- function(x,index){
    return(mean(x[index]))
}

## bootstrap with boot
## note that the estimates will be slightly different from those above because both are independent samples from the same object
## the difference between the estimate decreases as the number of bootstrap samples increases
x_r <- boot(data=x, mean_index, R=samples_r)

## original stat
x_r$t0

## average over the boot strapped stats
mean(x_r$t)

x_r




## let's estimate the bootstrapped standard error for the median statistic
x_r <- lapply(1:samples_r, function(i){
    x_r <- x[sample(1:length(x),size=sim_n,replace=TRUE)]
    return(median(x_r)) ## note this is the median now
})

## original estimate
median(x)


## bootstrap estimate
mean(unlist(x_r))
sd(unlist(x_r))

## graph the distribution
truehist(unlist(x_r))

## bootstrap estimate with the boot function
median_index <- function(x,index){
    return(median(x[index]))
}

x_r <- boot(data=x, median_index, R=samples_r)

## original stat
x_r$t0

## average over the boot strapped stats
mean(x_r$t)





## let's estimate the bootstrapped standard error for the median statistic
x_r <- lapply(1:samples_r, function(i){
    x_r <- x[sample(1:length(x),size=sim_n,replace=TRUE)]
    return(sum(x_r)) ## note this is the median now
})

## original estimate
sum(x)

## bootstrap estimate
mean(unlist(x_r))
sd(unlist(x_r))



