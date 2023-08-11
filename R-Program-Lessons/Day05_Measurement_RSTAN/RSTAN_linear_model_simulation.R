## RSTAN_linear_model_simulation.R
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
## For this R tutorial we will draw random samples from the normal distribution using the STAN program. These parameters will be estimated based on the likelihood function that links them to the data contained in the y and x variables that are simulated observed data. The model produces the slope and intercept from a standard linear model, which is also estimated using lm() in R.
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
        vector[n] x;
    }
    parameters {
        // declared the parameters in memory
        real alpha;
        real beta;
        real<lower=0> sigma;
    }
    model {
        // priors (these are variances not precision)
        //alpha ~ normal(0,10);
        //beta ~ normal(0,10);

        // likelihood (link data to some combination of parameters and more data)
        for(i in 1:n){
            y[i] ~ normal(alpha + beta * x[i], sigma);
        }
    }
    generated quantities {
        // posterior predictions
        vector[n] y_predict;

        // the loop is necessary within the generated quantities block
        for(i in 1:n){
            y_predict[i] = normal_rng(alpha + beta * x[i], sigma);
        }
    }
"
## -------------------------------------------------- #


## set data for simulation
n <- 100
x <- rnorm(n,0,1)
alpha <- 1.25
beta <- 2.50

## simulate a dependent variable with normally distribtued error using the data and parameter values defined above
error <- rnorm(n)
y <- alpha + beta * x + error

plot(x=x, y=y)

## fit linear model
summary(lm(y~x))

## create data list
data_list <- list(y = y, x=x, n=n)

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

## there are number of methods to subset and summarize parameters
## keep in mind that the output object is a list that contains vectors or matrices of of posterior estimates for each of the named parameter defined in the model statement above
## lapply (list-apply) a function to all of the objects in the list
lapply(output, mean)[1:3]
lapply(output, sd)[1:3]

## create a matrix using some of the named slots in the list
model_parameters <- as.matrix(fit, pars = c("alpha", "beta", "sigma"))
model_predictions <- as.matrix(fit, pars = "y_predict")

dim(model_parameters)
names(model_parameters)

plot(model_parameters[1:500,2], type="l", col=1)
lines(model_parameters[501:1000,2], type="l", col=2)
lines(model_parameters[1001:1500,2], type="l", col=3)
lines(model_parameters[1501:2000,2], type="l", col=4)


## check the dimensions (they should be the same)
dim(model_predictions)
dim(output$y_predict)

## plot the simulated y variable and the estimated posterior means
plot(y, apply(model_predictions,2,mean))



