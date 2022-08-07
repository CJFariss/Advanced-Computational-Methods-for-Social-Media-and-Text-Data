## R_Demo_Simulation_Learning_Probability_Distributions.R
##########################################################################
## INSTRUCTOR: Christopher Fariss
## COURSE NAME: Advanced Computational Methods for Social Media and Textual Data (2F)
## University of Essex Summer School 2021
##
## Please e-mail me if you find any errors or have and suggestions
## e-mail: cjf0006@gmail.com
## e-mail: cjfariss@umich.edu
##########################################################################
## Introduction to tutorial:
## For this R tutorial, we will learn about:
##
## (1) Discrete probability distributions for discrete events (e.g., a coin flip; rolling a 6-sided dice).
## (2) Continuous probability distributions (e.g., SAT, GRE or LSAT scores; human weight at birth; distance from the center of the board for the beanbag toss or cornhole game)
##
##
##########################################################################

## first we will construct a density function using the now familiar sample() function

## let's simulate the roll of a D6
library(MASS)

## total number of rolls
## try changing this value from 100 to 1000 to 10000 to 100000
sim_n <- 100
sim_n

values <- sample(c(1,2,3,4,5,6), size=sim_n, replace=T)
values

table(values)
table(values)/sim_n

par(mfrow=c(1,2), mar=c(4,4,2,.5)) ## as always ?par
barplot(table(values), names.arg=c(1,2,3,4,5,6), space=0, main="Frequency of D6 Rolls")
abline(h=sim_n/6, col=2, lwd=2)
barplot(table(values)/sim_n, names.arg=c(1,2,3,4,5,6), space=0, main="Proportion of D6 Rolls")
abline(h=1/6, col=2, lwd=2)

## what is the probability of getting 1 of the numbers on our fair D6?
## this is a probability density function for our D6 roll.
par(mfrow=c(1,1), mar=c(4,4,2,.5))
#par(mfrow=c(1,1), mar=c(6,6,4,4))
barplot(rep(1/6, 6), names.arg=c(1,2,3,4,5,6), space=0, main="Proportion of D6 Rolls", ylim=c(0,1/6))
abline(h=1/6, col=2, lwd=2)

##########################################################################
##
## Bernoulli distribution
##
## The Bernoulli distribution is a special case of the Binomial distribution
##
## The sample space S for the Bernoulli distribution, is defined to take one element.
## There are two probabilities estimated by the distribution p and 1-p.
## The probability distribution for p is in reference to when the variable y=1 and 1-p is in reference to when y=0.
##
## discrete range of the observed binary variable: [0,1]
## distribution function: p^y * (1-p)^(1-y)
## parameters: p is a continuous, real value parameter that ranges from 0 to 1,
## it is interpreted as the probability of success of a single trial (success is when y=1)
##
## R function names (all with size argument set to 1):
## dbinom
## pbinom
## qbinom
## rbinom
##
## parameters:
## formula for the mean: p
## formula for the variance: p*(1-p)
##
## let's define our own Bernoulli function
## the argument p can take real (continuous) values between 0 and 1
## the argument y can take discrete values of 0 or 1
Bernoulli_function <- function(p,y){
    p^y * (1-p)^(1-y)
}

## call our user defined function
Bernoulli_function(p=.5, y=1)
Bernoulli_function(p=.5, y=0)
Bernoulli_function(p=.25, y=1)
Bernoulli_function(p=.25, y=0)

## use R's based function for a Bernoulli distribution
dbinom(1,size=1,.5)
dbinom(0,size=1,.5)
dbinom(1,size=1,.25)
dbinom(0,size=1,.25)

## graph the probability disribution
barplot(dbinom(x=1, size=1, prob=0.5), names.arg=c(1), ylim=c(0,1), ylab="Pr(X=1)", xlab="x")

## generate a vector with a 0 and 1
values <- dbinom(x=c(0,1), size=1, prob=0.25)
values

barplot(values, names.arg=c(0,1), ylim=c(0,1), ylab="Pr(X=x)", xlab="x")

values <- dbinom(x=c(0,1), size=1, prob=0.95)
barplot(values, names.arg=c(0,1), ylim=c(0,1), ylab="Pr(X=x)", xlab="x")

values <- dbinom(x=c(0,1), size=1, prob=0.05)
barplot(values, names.arg=c(0,1), ylim=c(0,1), ylab="Pr(X=x)", xlab="x")


##########################################################################
##
## Binomial distribution
##
## The sample space S for the Bernoulli distribution, is defined to take one element. 
## There are two probabilities estimated by the distribution p and 1-p. 
## The probability distribution for p is in reference to when the variable y=1 and 1-p is in reference to when y=0.
##
## discrete range of the observed binary variable: [0,N]
## distribution function: choose(N, y) * p^y * (1-p)^(N-y)
## parameters: p is a continuous, real value parameter that ranges from 0 to 1, it is interpreted as the probability of success of a single trial (success is when y=1). when N > 1, it is interpreted as the probability of success for each individual N trial
##
## choose(N, y) is the binomial coefficient and is defined as N!/(y!(N − y)!).
## This turns into a very large number as N increases.
## It is used to normalize (bound the probability for a binomial outcome) to range from 0 to 1.
## It is also called the normalization coefficient.
##
## R function names (all with size argument set to N):
## dbinom
## pbinom
## qbinom
## rbinom
##
## parameters:
## formula for the mean: N*p
## formula for the variance: N*p*(1-p)
##
## let's define our own Bernoulli function
## the argument p can take values between 0 and 1
## the argument y can take discrete values of 0 or 1
Binomial_function <- function(p,y,N){
    (factorial(N)/(factorial(y) * factorial(N-y) )) * p^y * (1-p)^(N-y)
}

## quick reminder on factorials
## factorials are just the product of all the integer values in a sequence starting from the largest number in the seqence down to 1
factorial(3) # 3! written out in math but this won't work in R
prod(3:1)
prod(seq(from=3,to=1,by=-1))
3*2*1

factorial(4)
prod(4:1)
prod(seq(4,1))
4*3*2*1

## careful with this rule though
factorial(0)
prod(0)

## call our user defined function
Binomial_function(p=.5, y=2, N=2)
Binomial_function(p=.5, y=1, N=2)
Binomial_function(p=.5, y=0, N=2)
Binomial_function(p=.25, y=2, N=2)
Binomial_function(p=.25, y=1, N=2)
Binomial_function(p=.25, y=0, N=2)

## use R's based function for a Binomial distribution
dbinom(2,size=2,.5)
dbinom(1,size=2,.5)
dbinom(0,size=2,.5)
dbinom(2,size=2,.25)
dbinom(1,size=2,.25)
dbinom(0,size=2,.25)

## write out binomail function using the choose() function instead of factorial
Binomial_function_2 <- function(p,y,N){
    choose(N,y) * p^y * (1-p)^(N-y)
}
Binomial_function_2(p=.5, y=2, N=2)
Binomial_function(p=.5, y=2, N=2)
dbinom(2,size=2,.5)

sim_n <- 10000
coin_flip_trial1 <- sample(c(0,1),size=sim_n, replace=TRUE)
coin_flip_trial2 <- sample(c(0,1),size=sim_n, replace=TRUE)
table(coin_flip_trial1)/sim_n
table(coin_flip_trial2)/sim_n

table(coin_flip_trial1 + coin_flip_trial2)/sim_n

barplot(table(coin_flip_trial1 + coin_flip_trial2)/sim_n, names.arg=c(0,1,2), ylim=c(0,1), ylab="Pr(X=x)")


values <- dbinom(x=c(0,1,2), size=2, prob=0.5)
values
par(mfrow=c(1,1))
barplot(values, names.arg=c(0,1,2), ylim=c(0,1), ylab="Pr(X=x)")

values <- dbinom(x=c(0,1,2), size=2, prob=1/3)
values
barplot(values, names.arg=c(0,1,2), ylim=c(0,1), ylab="Pr(X=x)")

values <- dbinom(x=c(0,1,2,3,4,5,6), size=6, prob=1/2)
values
barplot(values, names.arg=c(0:6), ylim=c(0,1), ylab="Pr(X=x)")

values <- dbinom(x=c(0,1,2,3,4,5), size=5, prob=1/2)
values
barplot(values, names.arg=c(0:5), ylim=c(0,1), ylab="Pr(X=x)")


values <- dbinom(x=0:10, size=10, prob=.5)
values
barplot(values, names.arg=c(0:10), ylim=c(0,1), ylab="Pr(X=x)")


values <- dbinom(x=0:10, size=10, prob=.9)
values
barplot(values, names.arg=c(0:10), ylim=c(0,1), ylab="Pr(X=x)")


## here is where the Bonus (optional) questions start

##########################################################################
## Additional probability distributions for discrete variables (integers that are usually strictly positive)
##########################################################################
##
##########################################################################
## Poisson distribution
##
## R function names:
## dpois
## ppois
## qpois
## rpois
##
##########################################################################
## geogetric distribution
##
## R function names:
## dgeom
## pgeom
## qgeom
## rgeom
##
##########################################################################
## negative binomial distribution
##
## R function names:
## dnbinom
## pnbinom
## qnbinom
## rnbinom
##
##########################################################################
## hypergeometric distribution
##
## R function names:
## dhyper
## phyper
## qhyper
## rhyper
##
##########################################################################
## multinomial distribution
##
## R function names:
## rmultinom
## dmultinom
##
##########################################################################
## continuous probability distributions usually called density functions
##########################################################################
##
## uniform distribution
##
## The uniform distribution is a continuous distribution
##
## The sample space S for the uniform distribution, is defined so that the density for any value is the same or uniform over the range of possible values that any one element can take. Instead of directly estimating probabilities as we did above, we will instead estimate density.
##
## What is density? Density is the relative probability of sampling an event a specific point along the range of possible values defined by the function.
##
## In other words, the uniform distribution with range a to b, denoted U (a, b), has a constant probability density of 1/(b−a) for a <= y <= b and zero probability elsewhere. The standard uniform is defined so that a=0 and b=1: U(0,1),
##
## range a <= y <= b
## distribution function: 1/(b − a)
## parameters: a and b
## R function names:
## dunif
## punif
## qunif
## runif
##
## parameters:
## formula for the mean: (a + b)/2
## formula for the variance: ((b - a)^2)/12
##
## let's define our own uniform function
uniform_function <- function(x,a,b){
    ifelse(x>=a & x<=b, 1/(b - a), 0)
}

## call our user defined function
uniform_function(1,0,1)
uniform_function(1,0,2)
uniform_function(1,-1,1)
uniform_function(1,-10,10)

uniform_function(-8,0,1)
uniform_function(-8,0,2)
uniform_function(-8,-1,1)
uniform_function(-8,-10,10)

## use R's based function for a uniform distribution
dunif(1,0,1)
dunif(1,0,2)
dunif(1,-1,1)
dunif(1,-10,10)

dunif(-8,0,1)
dunif(-8,0,2)
dunif(-8,-1,1)
dunif(-8,-10,10)

## we can use the p or probability version of the uniform function built into R to answer to the following question:
## What is the probability of observing a value between 0 and 0.5 for a uniform random variable that spans the range 0 to 1?
punif(0.5,0,1)

punif(0.25,0,1)

## What is the probability of observing a value between 0.25 and 0.5 for a uniform random variable that spans the range 0 to 1?
punif(0.5,0,1) - punif(0.25,0,1)

## Graph the Uniform density function using the curve() function
curve(expr=dunif, ylab="Pr(X=x)", type="l", xlim=c(0,1), ylim=c(0,1.05), lwd=3, col="navy", main="U(0,1) Density")

curve(expr=dunif, n=10001, from=-2,to=2, ylab="Pr(X=x)", type="l", xlim=c(-2,2), ylim=c(0,1.05), lwd=3, col="navy", main="U(0,1) Density")

curve(expr=dunif(x, min = 0, max = 1), n=10001, from=-2,to=2, ylab="Pr(X=x)", type="l", xlim=c(-2,2), ylim=c(0,1.05), lwd=3, col="navy", main="U(0,1) Density")

curve(expr=dunif(x, min = -1, max = 1), n=10001, from=-2,to=2, ylab="Pr(X=x)", type="l", xlim=c(-2,2), ylim=c(0,1.05), lwd=3, col="navy", main="U(-1,1) Density")

curve(expr=dunif(x, min = -2, max = 2), n=10001, from=-2,to=2, ylab="Pr(X=x)", type="l", xlim=c(-2,2), ylim=c(0,1.05), lwd=3, col="navy", main="U(-2,2) Density")

curve(expr=dunif(x, min = -2, max = 2), n=10001, from=-10,to=10, ylab="Pr(X=x)", type="l", xlim=c(-10,10), ylim=c(0,1.05), lwd=3, col="navy", main="U(-2,2) Density")

curve(expr=dunif(x, min = -2, max = 2), n=10001, from=-100,to=100, ylab="Pr(X=x)", type="l", xlim=c(-100,100), ylim=c(0,1.05), lwd=3, col="navy", main="U(-2,2) Density")


## how many of the draws are very close to the middle of the normal distribution? (hint: what is the proportion)
sim_n <- 10000
random_draws <- runif(sim_n,0,1)
summary(random_draws)

table(random_draws>=0 & random_draws<=.25)/sim_n

table(random_draws>=0 & random_draws<=.5)/sim_n

table(random_draws>=.4 & random_draws<=.5)/sim_n

table(random_draws>=0 & random_draws<=1)/sim_n

## we can calculate the above with a p-type function in R
## the p-type function for a density function tells us the density or probability of a function at a given value along the x-axis
punif(.25) - punif(0)

punif(.5) - punif(0)

punif(1) - punif(0)

punif(1) - punif(-1)

punif(.5) - punif(.4)


##########################################################################
##
## normal distribution
##
## The normal distribution is a continuous distribution
##
## The sample space S for the normal distribution, is defined so that the density for any value along the real number line from negative infinity to positive infinity. Instead of directly estimating probabilities as we did above, as with the uniform distribution we will instead estimate density.
##
## What is density? Density is the relative probability of sampling an event a specific point along the range of possible values defined by the function.
##
## Unlike the uniform distribution the density value for a normal distribution varies (is not uniform) for different values along the real number line.
##
##
## range:
## distribution function: 1/(sigma*sqrt(2*pi)) * exp(-(y - mu)^2/2*sigma^2)
## parameters: the mean mu and the standard deviation sigma (or the variance sigma^2)
## R function names:
## dnorm
## pnorm
## qnorm
## rnorm
##
## parameters:
## formula for the mean: mu
## formula for the variance: sigma^2
##
## let's define our own uniform function
normal_density <- function(y, mu, sigma){
    1/(sigma*sqrt(2*pi)) * exp(-(y - mu)^2/2*sigma^2)
}

## call the user defined normal density function
normal_density(y=0, mu=0, sigma=1) ## standard normal density mu=0, sd=1
normal_density(y=0, mu=1, sigma=1)
normal_density(y=-1, mu=1, sigma=1)
normal_density(y=1, mu=1, sigma=1)
normal_density(y=1.5, mu=1, sigma=1)

## use R's based function for a normal distribution
dnorm(0, mean=0, sd=1)
dnorm(0, mean=1, sd=1)
dnorm(-1, mean=1, sd=1)
dnorm(1, mean=1, sd=1)
dnorm(1.5, mean=1, sd=1)

## Remember that for a normal distribution the probablity of observing a specific event or value of a variable
## changes depending on where along the real number line the value resides.

## we can use the p or probability verison of the normal function built into R to answer to the following question:

## What is the probability of observing a value between negative infinity and 0 for a normally distributed random variable with mean=0 and standard deviation=1 ?
pnorm(0, mean=0, sd=1)

## What is the probablity of observing a value between -1 and 0 for a normally distributed random variable with mean=0 and standard deviation=1 ?
pnorm(0,0,1) - pnorm(-1,0,1)

## let's graph this to see what is happening
## load packages
pkgs <- c("MASS")
invisible(sapply(pkgs, require, character.only = TRUE))

## create sequence of real numbers, these are a set of possible values that are estiamte of the mean can take
x <- seq(-3,3,.001)
x
length(x)

## set mean and variance parameters
mu <- 0
sigma <- 1
sigma_pow2 <- sigma^2

## calculate the density using the normal distribution function
## density is a probability measure
x_density <- 1/(sigma*sqrt(2*pi)) * exp(-(x - mu)^2/2*sigma^2)

## calculate the density using the normal distribution function dnorm() which is built into R
x_dnorm <- dnorm(x, mean=mu, sd=sigma)

## plot the two estimated densities to verify that they are the same
par(mfrow=c(1,1))
plot(x_density, x_dnorm)

## plot the x_density variable along each value of x definied in the sequence above:
plot(x,x_density, xlab="random variable", ylab="density")

sum(x_density)/sum(x_density)  ## this should approach as we add more values to the sequence 1 either by decreasing the interval between values in the sequence or by increasing the total range of the seqence
sum(x_density[x<=0])/sum(x_density)
sum(x_density[x<=0 & x>=-1])/sum(x_density)

pnorm(0,0,1)
pnorm(0,0,1) - pnorm(-1,0,1)


## fill in the sequence more so that the values converge on the true values from the pnorm functions
x <- seq(-6,6,.001)
x_density <- 1/(sigma*sqrt(2*pi)) * exp(-(x - mu)^2/2*sigma^2)
sum(x_density)/sum(x_density)
sum(x_density[x<=0])/sum(x_density)
sum(x_density[x<=0 & x>=-1])/sum(x_density)


## Graph the Normal density function using the curve() function
curve(expr=dnorm, ylab="Pr(X=x)", type="l", xlim=c(-4,4), ylim=c(0,1), lwd=3, col="navy", main="N(0,1) Density")

curve(expr=dnorm, ylab="Pr(X=x)", type="l", xlim=c(-8,8), ylim=c(0,1), lwd=3, col="navy", main="N(0,1) Density")

curve(expr=dnorm(x, mean=0, sd=2), ylab="Pr(X=x)", type="l", xlim=c(-4,4), ylim=c(0,1.05), lwd=3, col="navy", main="N(0,2^2) Density")

curve(expr=dnorm(x, mean=0, sd=2), ylab="Pr(X=x)", type="l", xlim=c(-8,8), ylim=c(0,1.05), lwd=3, col="navy", main="N(0,2^2) Density")

curve(expr=dnorm(x, mean=0, sd=1/2), ylab="Pr(X=x)", type="l", xlim=c(-4,4), ylim=c(0,1.05), lwd=3, col="navy", main="N(0,.5^2) Density")

curve(expr=dnorm(x, mean=0, sd=1/2), ylab="Pr(X=x)", type="l", xlim=c(-8,8), ylim=c(0,1.05), lwd=3, col="navy", main="N(0,.5^2) Density")


## graph some randomly generated numbers

par(mfrow=c(2,2))

random_draws <- rnorm(10)
truehist(random_draws)
curve(expr=dnorm, xlim=c(-3,3), col="darkorange", add=TRUE, lwd=2)

random_draws <- rnorm(100)
truehist(random_draws)
curve(expr=dnorm, xlim=c(-3,3), col="darkorange", add=TRUE, lwd=2)

random_draws <- rnorm(1000)
truehist(random_draws)
curve(expr=dnorm, xlim=c(-3,3), col="darkorange", add=TRUE, lwd=2)

random_draws <- rnorm(10000)
truehist(random_draws)
curve(expr=dnorm, xlim=c(-3,3), col="darkorange", add=TRUE, lwd=2)

## how many of the draws are very close to the middle of the normal distribution?
## (hint: what is the proportion)
sim_n <- 10000
random_draws <- rnorm(sim_n)

table(random_draws>-1 & random_draws<1)/sim_n

table(random_draws>-2 & random_draws<2)/sim_n

table(random_draws>-3 & random_draws<3)/sim_n

## we can calculate the above with a p-type function in R
## the p-type function for a density function tells us the density or probability of a function at a given value along the x-axis
pnorm(1) - pnorm(-1)

pnorm(2) - pnorm(-2)

pnorm(3) - pnorm(-3)


## more optional bonus problems

##########################################################################
## additional continuous probability distributions usually called density functions
##########################################################################
##
## Student's t-distribution
##
## R function names:
## dt
## pt
## qt
## rt
##
##########################################################################
## exponential distribution
##
## R function names:
## dexp
## pexp
## qexp
## rexp
##
##########################################################################
## chi-squared disribution
##
## R function names:
## dchisq
## pchisq
## qchisq
## rchisq
##
##########################################################################
## f-distribution
##
## R function names:
## df
## pf
## qf
## rf
##
##########################################################################
## gamma distribution
##
## R function names:
## dgamma
## pgamma
## qgamma
## rgamma
##
##########################################################################
## beta distributions
##
## R function names:
## dbeta
## pbeta
## qbeta
## rbeta
