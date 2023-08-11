## RSTAN_normal_distribution_simulation.R
##########################################################################
## INSTRUCTOR: Christopher Fariss
## COURSE NAME: Advanced Computational Methods for Social Media and Textual Data (3B)
## University of Essex Summer School 2023
##
## Date: 2023-08-11
##
## Please e-mail me if you find any errors or have and suggestions
## e-mail: cjf0006@gmail.com
##
##########################################################################
##
## Introduction to tutorial:
##
## For this R tutorial we will draw random samples from the normal distribution using the STAN program. 
## This program is equivalent to using the rnorm function in R.
##
##########################################################################


## load library
library(rstan) # load rstan library
library(MASS) # load library with truehist function


## -------------------------------------------------- #
## define STAN model
## -------------------------------------------------- #
model <- "

parameters {
    real mu;
}

model {
    mu ~ normal(0,1);
}
"
## -------------------------------------------------- #


## set time start variable
time1 <- Sys.time()

## fit stan model
fit <- stan(model_code = model, iter = 1000, chains = 4)

## calculate the duration of the program file up to this point
print(Sys.time() - time1)

## extract draws from stan model object (creates a list object)
output <- extract(fit, permuted = TRUE)

## print names of each element/slot in the list
names(output)

## print model fit object
fit

## there are number of methods to subset and summarize parameters
## keep in mind that the output object is a list that contains vectors or matrices of posterior estimates for each of the named parameter defined in the model statement above
## lapply (list-apply) a function to all of the objects in the list
lapply(output, mean)
lapply(output, sd)

## create a matrix using some of the named slots in the list
model_parameters <- as.matrix(fit, pars = c("mu"))

## check the dimensions (they should be the same)
length(output$mu)

## make a nice plot
#par(mfrow=c(2,2))
truehist(output$mu)


