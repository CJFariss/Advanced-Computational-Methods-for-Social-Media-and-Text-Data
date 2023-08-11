## R_Demo_logit_transformation_latent_variable.R
##########################################################################
## INSTRUCTOR: Christopher Fariss
## COURSE NAME: Advanced Computational Methods for Social Media and Textual Data (3B)
## University of Essex Summer School 2023
##
## Date: 2023-08-11
##
## Please e-mail me if you find any errors or have and suggestions (either email is fine)
## e-mail: cjf0006@gmail.com
## e-mail: cjfariss@umich.edu
##
##########################################################################
##
## Introduction to tutorial:
##
## Think about a latent variable model without actually running one.
##
## This program is a visual look at the relationship between values along a fixed sequence that represents the distribution of the latent variable and its relationship to binary variables that are equally likely (50%-50% change).
##
## In the IRT framework, there is a latent trait theta_i.
##
## Where the subscript i = 1,... ,N indicates multiple units.
##
## y_ik is the observed value for item k for unit i.
##
## For each item alpha_k and beta_k are also estimated.
##
## alpha_k continues to act as "difficulty" parameters, or threshold that benchmarks how likely an indicator is to be observed relative to the values of the latent trait.
##
## In this formulation, this is analogous to an intercept in a traditional logistic regression model.
##
## beta_k, often referred to as the "discrimination" parameters and is the analogue of a slope coefficient.
##
##########################################################################

## make theta a deterministic sequence (we will use each value below to calculate a range of probabilities as a function of these values and the item-discrimination and item-difficulty parameters)
theta <- seq(-5,5,.1)
n <- length(theta)

## set strength of relationship (i.e., the item discrimination)
##
## try setting this value to an positive real number
##
## as beta increases the logit curve will converge towards a set function
##
## the step function will reside over the inflection point of the logit curve
##
## the inflection point is always -(alpha/beta)
##
## because of this identity, we have to expand the intercepts below by a factor of beta
##
## the expansion below is necessary to fix the curve over the range of the standard normal density set for x above
##
#beta <- .1
#beta <- .5
#beta <- 1
#beta <- 2
#beta <- 3
#beta <- 4
#beta <- 5
#beta <- 10
#beta <- 100
#beta <- 1000
#beta <- 10000
##
## as beta approach infinity, the value along x becomes perfectly predictive of the y variables
##
#beta <- 100000


## set beta here (pick three values for beta, the item discrimination parameter)
beta <- c(.5,1,3,5)

par(mfrow=c(4,3), mar=c(5,3,.5,.5))
for(j in 1:length(beta)){
    ## set intercepts * an expansion factor, which is the item discrimination from above
    ## values correspond exactly to the position along the standard normal x variable
    ## specifically, these are the position along x at which point at which Pr(y=1)=.5
    alpha <- beta[j]*c(-2,-.5,2)
    
    # linear terms of the model
    # transform the linear xb terms using the logit function into a probability
    xb <- p <- y <- matrix(NA, nrow=n, ncol=length(alpha))
    for(i in 1:length(alpha)){
        xb[,i] <- alpha[i] + beta[j] * theta
        p[,i] <- 1 / (1 + exp(-xb[,i]))
        y[,i] <- rbinom(n, size=1, prob=p[,i])
    }
    
    for(i in 1:length(alpha)){
        ## graph theta values along the the x-axis
        ## x values are projected onto the probability of y using the inverse logit function of xb
        plot(theta, p[,i], xlim=c(-4.0,4.0), ylim=c(0,1), xaxt="n", xlab="", type="l", lwd=2, col=grey(.5), ylab="")
        if(j==3)mtext(side=1, expression(theta), cex=1.25, line=3)
        if(i==1)mtext(side=2, expression("Probability Y=1"), cex=1.25, line=3)
        A <- alpha[i]
        B <- beta[j]
        text(-3.25,.95, substitute(paste(beta, " = ", B), list(B = B)), cex=1.5)
        text(-3.25,.85, substitute(paste(alpha, " = ", A), list(A = A)), cex=1.5)
        axis(side=1, at=-5:5, cex.axis=1.25)
        abline(h=0.5)
        abline(v=-(alpha[i]/beta[j]), col=2)
        #abline(v=-((alpha[i]+log(2.75))/beta[j]), col=2, lwd=.5, lty=2)
        #abline(v=-((alpha[i]-log(2.75))/beta[j]), col=2, lwd=.5, lty=2)
    }
}



