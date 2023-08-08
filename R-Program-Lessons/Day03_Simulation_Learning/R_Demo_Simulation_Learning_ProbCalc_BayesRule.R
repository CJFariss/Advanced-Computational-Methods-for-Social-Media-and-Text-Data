## R_Demo_Simulation_Learning_ProbCalc_BayesRule.R
##########################################################################
## INSTRUCTOR: Christopher Fariss
## COURSE NAME: Advanced Computational Methods for Social Media and Textual Data (3B)
## University of Essex Summer School 2023
##
## Date: 2023-08-08
##
## Please e-mail me if you find any errors or have and suggestions
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


A <- c(0,1,0,1)
B <- c(0,0,0,1)


ProbCalc <- function(A,B){

  len <- length(A)
  data <- as.data.frame(cbind(A,B, A*B, A+B-A*B))
  names(data) <- c("A", "B", "A and B", "A or B")

  pr.A <- sum(A)/len
  pr.B <- sum(B)/len
  pr.NotA <- 1 - pr.A
  pr.NotB <- 1 - pr.B
  pr.A.B <- sum(A*B)/len
  pr.NotA.B <- sum(abs(A-1)*B)/len
  pr.A.NotB <- sum(A*abs(B-1))/len
  pr.NotA.NotB <- sum(abs(A-1)*abs(B-1))/len

  pr.A.or.B <- pr.A + pr.B - pr.A.B
  
  pr.A.condB <- pr.A.B / pr.B
  pr.B.condA <-pr.A.B / pr.A
  
  out <- list(data=data, pr.A=pr.A, pr.B=pr.B, pr.NotA=pr.NotA, pr.NotB=pr.NotB, pr.A.B=pr.A.B, pr.NotA.B=pr.NotA.B, pr.A.NotB=pr.A.NotB, pr.NotA.NotB=pr.NotA.NotB,  pr.A.or.B= pr.A.or.B, pr.A.condB=pr.A.condB, pr.B.condA=pr.B.condA)
  return(out) 

}

ProbCalc(A,B)

ProbTabCalc <- function(A,B){

  tab <- table(A,B)/length(A)

  pr.A <- sum(tab[A==1])
  pr.B <- sum(tab[B==1])
  pr.NotA <- sum(tab[A==0])
  pr.NotB <- sum(tab[B==0])
  pr.A.B <- sum(tab[A==1 & B==1])
  pr.NotA.B <- sum(tab[A==0 & B==1])
  pr.A.NotB <- sum(tab[A==1 & B==0])
  pr.NotA.NotB <- sum(tab[A==0 & B==0])

  pr.A.or.B <- sum(tab[A==1 | B==1])
  
  pr.A.condB <- pr.A.B / pr.B
  pr.B.condA <-pr.A.B / pr.A

  out <- list(table=tab, pr.A=pr.A, pr.B=pr.B, pr.NotA=pr.NotA, pr.NotB=pr.NotB, pr.A.B=pr.A.B, pr.NotA.B=pr.NotA.B, pr.A.NotB=pr.A.NotB, pr.NotA.NotB=pr.NotA.NotB,  pr.A.or.B= pr.A.or.B, pr.A.condB=pr.A.condB, pr.B.condA=pr.B.condA)
    return(out)

}

ProbTabCalc(A,B)


