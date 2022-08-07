#### R_Demo_Simulation_Inference_naive_Bayes.R
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
## Introduction to tutorial:
##
## Overview of Bayes rule and the calculation of conditional probability. Introduces the naive Bayes classifier function from the e1071 library.
##
## P(outcome | evidence) = P(outcome) * P(evidence | outcome) / P(evidence)
##
## Below is the Bayes’ Theorem:
## P(A | B) = P(A) * P(B | A) / P(B)
##
## Which can be derived from the general multiplication formula for AND events:
## P(A and B) = P(A) * P(B | A)
## P(B | A) = P(A and B) / P(A)
## P(B | A) = P(B) * P(A | B) / P(A)
## P(y|x) = P(x|y) * P(y) / P(x)
## P(x|y) = P(x AND y) / P(x)
##
## Pr(A[1] = Pr(y==0)
## Pr(A[2] = Pr(y==1)
## Pr(B | A[1]) = Pr(Data | y==0)
## Pr(B | A[2]) = Pr(Data | y==1)
##
##########################################################################


## load libraries
library(e1071)
library(LaplacesDemon)


## example code from BayesTheorem() function
PrA <- c(0.75,0.25)
PrBA <- c(6/9, 5/7)
BayesTheorem(PrA, PrBA)


## create fake data
n <- 10
x <- c(rep(0,n/2), rep(1,n/2))
y <- c(0,0,0,1,1,0,0,1,1,1)

## inspect data
cbind(y,x)

## inspect tabulation of data
table(y,x)


## calculate the probability of the evidence/data
PrX <- NA
PrX[1] <- sum(as.numeric(x==1)) / n
PrX[2] <- sum(as.numeric(x==1)) / n

## calculate the probability of the outcome
PrY <- NA
PrY[1] <- sum(as.numeric(y==0))/n
PrY[2] <- sum(as.numeric(y==1))/n
PrY

## calculate the probability of the data conditional on the value of y (the likelihood)
PrXY<- NA
PrXY[1] <- sum(x[y==0])/length(as.numeric(x[y==0]))
PrXY[2] <- sum(x[y==1])/length(as.numeric(x[y==1]))
PrXY

## apply Bayes Rule
PrXY * PrY / PrX

## apply Bayes Rule with BayesTheorem() function
BayesTheorem(PrA=PrY, PrBA=PrXY)


## apply Bayes Rule with naiveBayes() function
fit <- naiveBayes(y~x, data=data.frame(y,x))
fit



