## RSTAN_logistic_regression_simulation.R
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
## For this R tutorial we will simulate a binary dependent variable and then estimate the parameters that generate the variable. These parameters will be estimated based on the likelihood function that links them to the data contained in the y and x variables that are simulated observed data. The model produces the slope and intercept from a standard logistic regression model, which is also estimated using glm() in R.
##
##########################################################################

## load library
library(rstan) # load rstan library
library(MASS) # loaed library with truehist function

## -------------------------------------------------- ##
## define STAN model
## -------------------------------------------------- ##
model <- "
    data {
        // declared the data in memory
        int<lower=0> n;
        int<lower=0, upper=1> y[n];
        vector[n] x;
    }
    // declared the parameters in memory
    parameters {
        real alpha;
        real beta;
    }
    model {
        // priors (these are variances not precision)
        //alpha ~ normal(0,10);
        //beta ~ normal(0,10);
    
        // likelihood (link data to some combination of parameters and more data)
        y ~ bernoulli_logit(alpha + beta * x);
    }
generated quantities {
    // posterior predictions
    vector[n] y_predict;
    
    // the loop is necessary within the generated quantities block
    for(i in 1:n){
        y_predict[i] = bernoulli_logit_rng(alpha + beta * x[i]);
    }
}

"
## -------------------------------------------------- ##


## simulate x1 and set the "true" population values alpha and beta
n <- 100
x <- rnorm(n,0,1)
alpha <- 1.25
beta <- 2.50

## systematic component of the model
xb <- alpha + beta * x

## transform the linear term xb using
## the inverse logit function
## so that theta is bound from 0 to 1
eta <- 1 / (1 + exp(-xb))

## generate the dependent variable y with probability inv.theta and measurment error from a Bernoulli trial
y <- rbinom(n, size=1, prob=eta)

table(y)

## create data list
data_list <- list(y = y, x=x, n=n)

## fit linear model
summary(glm(y~x, family=binomial(link="logit")))

## fit stan model
fit <- stan(model_code = model, data = data_list, iter = 1000, chains = 4)

## extract draws from stan model object
output <- extract(fit, permuted = TRUE)

## print names
names(output)

## there are number of methods to subset and summarize parameters
## keep in mind that the output object is a list that contains vectors or matrices of of posterior estimates for each of the named parameter defined in the model statement above
## lapply (list-apply) a function to all of the objects in the list
lapply(output, mean)
lapply(output, sd)

## tabulate the simulated binary dependent variable it should be very close to the mean value of the predicted y
table(y)

## create a matrix using some of the named slots in the list
model_parameters <- as.matrix(fit, pars = c("alpha", "beta"))
model_predictions <- as.matrix(fit, pars = "y_predict")

## check the dimensions (they should be the same)
dim(model_predictions)
dim(output$y_predict)

## plot the simulated y variable and the estimated posterior means
plot(apply(model_predictions,2,mean), y)



