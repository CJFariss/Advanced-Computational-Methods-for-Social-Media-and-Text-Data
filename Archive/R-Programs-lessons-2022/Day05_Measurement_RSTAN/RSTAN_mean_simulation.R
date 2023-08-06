## RSTAN_mean_simulation.R
##########################################################################
## INSTRUCTOR: Christopher Fariss
## COURSE NAME: A Crash Course in Measurement and Latent Variable Modeling with RSTAN
##
## Please e-mail me if you find any errors or have and suggestions
## e-mail: cjf0006@gmail.com
##
##########################################################################
##
## Introduction to tutorial:
##
## For this R tutorial we will use the normal distribution in STAN to estimate the mean value for an observed variable y. The parameter for the mean will be selected based on the likelihood function that links them to the data contained in the y variable.
##
##########################################################################

## load library
library(rstan) # load rstan library
library(MASS) # load library with truehist function

## -------------------------------------------------- #
## define STAN model as a character
## -------------------------------------------------- #
model <- "
    data {
        // declared the data in memory
        int<lower=0> n;
        vector[n] y;
    }
    parameters {
        // declared the parameters in memory
        real mu;
        real<lower=0> sigma;
    }
    model {
        // there are no prior statements for mu or sigma; 
        // by default the priors on the parameters are flat unless we provide more information (see the other examples)
        // likelihood (link data to some combination of parameters and more data)
        
        mu ~ normal(0,1);
        
        for(i in 1:n){
            y[i] ~ normal(mu, sigma);
        }
    }
    generated quantities {
        // posterior predictions
        vector[n] y_predict;

        // the loop is necessary within the generated quantities block
        for(i in 1:n){
            y_predict[i] = normal_rng(mu, sigma);
        }
    }
"
## -------------------------------------------------- #


## set data for simulation
#y <- 1:5
y <- rep(1:5,200)

n <- length(y)
y
n

## create data list
data_list <- list(y = y, n=n)
data_list

## set time start variable
time1 <- Sys.time()

# fit stan model
fit <- stan(model_code = model, data = data_list, iter = 1000, chains = 4)

## calcuate the duration of the program file up to this point
print(Sys.time() - time1)

## extract draws from stan model object (creates a list object)
output <- extract(fit, permuted = TRUE)

## print names of each element/slot in the list
names(output)

## print model fit object
fit

## lapply (list-apply) a function to all of the objects in the list
lapply(output, mean)
lapply(output, sd)

truehist(output$mu)

