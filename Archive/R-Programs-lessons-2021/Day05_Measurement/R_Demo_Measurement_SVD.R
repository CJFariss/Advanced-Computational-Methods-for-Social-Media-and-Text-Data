## R_Demo_Measurement_SVD.R
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
##
##########################################################################
##
## Introduction to tutorial:
##
## (1) Review some linear alegbra functions (inner and outer product functions and alternatives)
##
## (2) The Hilbert function (proofs are elsewhere)
##
## (3) Introduce Singular Value Decomposition (SVD)
##
## SVD is a linear factorization of a matrix that generalizes the eigen decomposition of a square normal matrix to any matrix via an extension of the polar decomposition.
##
## This is a lot! And beyond the scope of the course (it is still fun and interesting though).
##
## We could spend a couple weeks unpacking all of these ideas.
##
## R contains fast and efficient linear algebra functions (based in Fortran code from the 1950s/1960s) so it is easy to explore.
##
##########################################################################

## linear algebra is a just a set of rules/algorithms for combining numeric information in vectors and matrices


## generate a vector (use coordinates to start but can be any values)
x <- 1:5

## inner product from linear alegbra using the %*% operator
x %*% x

## why is it the "inner" product?
## because we sum the pairwise products of all values in the relevant dimension of the two objects
## inner product is equivalent to this rule:

## pairwise products from the vector x and the vector x
x*x

## sum of pairwise products from the vector x and the vector x (i.e., the inner product)
sum(x*x)

## outer product from linear alegbra using the %o% operator
x %o% x

## why is it the "outer" product?
## because we calculate the product of all pairs of coordinates in the relevant dimension of the two objects
## outer product function
outer(x,x,"*")

## alternative uses of the outer() function (apply the function to all row and column coordinates)
outer(x,x,"^")
outer(x,x,"+")
outer(x,x,"-")
outer(x,x,"/")

## now using the coordinates
outer(x-1, x, "+")

1/outer(x-1, x, "+")

## define the Hilbert function
## which uses the outer product (using the sum function)
## which is calculated using coordinates and (coordinate -1) pairs from a 1D vector of coordinates 1:n
Hilbert_func <- function(n) {
    i <- 1:n
    return(1 / outer(i - 1, i, "+"))
}

Hilbert_func(5)


## example from R
x <- Hilbert_func(9)[, 1:6]
x

## SVD
s <- svd(x)
s

D <- diag(s$d)
D

##
s$u %*% D %*% t(s$v) #  X = U D V'

##
t(s$u) %*% x %*% s$v #  D = U' X V


## let's try to reproduce SVD
n <- 100
theta <- rnorm(n,0,1)

## set parameters for each item
## alpha (the intercept) is the difficulty parameter or base-line probability of 1
## beta (the slope) is the discrimination parameter or the strength of the relationship
## between the estimated latent trait theta and the individual item

## item 1 is the most difficult and least informative item
alpha1 <- -1.000000
beta1 <- 1.000000

## item 2 is of medium difficulty relative to the two other items
## it is also the same level of informativeness as item1
alpha2 <- 0.000000
beta2 <- 1.000000

## item 3 is the least difficult item but is also the most informative
## these statements are each relative to the other items in the model
alpha3 <- 1.000000
beta3 <- 1.000000

## define k as the number of items
k <- 3

## linear terms of the item specific models f() that link the latent trait to the items
x1 <- alpha1 + beta1 * theta + rnorm(n)
x2 <- alpha2 + beta2 * theta + rnorm(n)
x3 <- alpha3 + beta3 * theta + rnorm(n)


## create matrix of observed items
x <- cbind(x1, x2, x3)

## recover theta via the decomposition of x with SVD
s <- svd(x)
D <- diag(s$d)

##
s$u %*% D %*% t(s$v) #  X = U D V'
t(s$u) %*% x %*% s$v #  D = U' X V

## comparing the singular values of U to theta
cor(s$u[,1], theta)
cor(s$u[,2], theta)
cor(s$u[,3], theta)

## contrast the correlations from the original observed variables (note the change)
cor(x[,1], theta)
cor(x[,2], theta)
cor(x[,3], theta)

