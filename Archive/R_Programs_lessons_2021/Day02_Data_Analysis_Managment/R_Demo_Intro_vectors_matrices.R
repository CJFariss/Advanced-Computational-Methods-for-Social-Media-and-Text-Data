## R_Demo_Intro_vectors_matrices.R
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
## For this R tutorial, we will learn how:
##  (1) how to create scalars, vectors, matrices, and arrays in R
##  (2) manipuate (rotate, flip, overwrite, append to) each of these data objects
##
##########################################################################



##########################################################################
## numeric scalars vectors
##########################################################################

## create a scalar object
s <- 2

## print to screen
s

##
s[1]

## s is a scalar, it is also a vector of length 1
## length of the object s
length(s)

## we can concatenate numbers to the scalar and create a vector by adding the second arugment to the right of the first argument
c(s,3)

## we can overwright the s object so that the new value concatentated to s is saved as a vector s
s <- c(s,3)

## print to screen
s

## print to screen values using the coordinates at which those values reside
s[1]
s[2]

## create a vector of length five
v <- c(1, 2, 3, 4, 5)
v

v[1]

## calculate the average of the vector
mean(v)
sum(v)/5
sum(v)/length(v)

## we can take the min or max of the vector too
min(v)
max(v)

## use subscript operator [] to get one element from the v vector we just created
v[1]
v[3]

## use a negative subscript to get all but that specific element from a vector
v[-1]

## use a vector of subscripts to get multiple elements from a vector
v[c(1,2)]

## use a vector of subscripts to exclude multiple elements from a vector
v[c(-3,-4,-5)]

## column bind using the cbind() function
## row bind using the rbind() function
## these two functions let us stack vectors together

## let's create a new numeric vector
a <- c(1,2,5.3,6,-2,4)

## stack the colums together as columns and as rows
cbind(a,a)
rbind(a,a)


## lets create and manipulate vectors

## lets create a seqeuence of values using the seq() function
## create a sequence of integers from 1 to 5
seq(from=1, to=5, by=1)

## we can create the same sequence as above using the : operator
1:5

5:1

## : only produces sequences so that the values are 1 unit distance from one another but the start and end values do not need to be integers
1.5:5.5

1.5:4

## we can use the seq() function from above to change the distance between values in the vector
seq(from=1, to=5, by=.5)

## instead of specifing the distance between values we can specify the number of units we wish to produce (i.e., the length of the vector)
seq(from=1, to=5, length.out=3)

seq(from=1, to=5, length.out=5)

seq(from=1, to=5, length.out=9)

seq(from=1, to=5, length.out=10)

seq(from=1, to=5, length.out=100)

## it might also be useful to generate a vector of the same value n number of times
## that is we might want to replicate a specific value n times using rep()
rep(1,10)

## we can replicate values from another vector
rep(c(1,2,3), times=2)

## notice the difference between using the times argument compared to the each argument
rep(c(1,2,3), each=2)





##########################################################################
## matrices and arrays
##########################################################################

## Note: see Matloff Chapter 3 (Matrices and Arrays) for more information on the material below

## a matrix is just a 2D array (note that byrow=FALSE by default so that the values are added one column at a time from left to right)
m <- matrix(NA, nrow=2, ncol=3)
m

m <- matrix( c(1,2,3,4,5,6), nrow=2, ncol=3)
m

## covert the matrix back into a vector (note the order of the values)
c(m)

## this is the same function as above but the values are added one row at at time from top to bottom
m <- matrix( c(1,2,3,4,5,6), nrow=2, ncol=3, byrow = TRUE)
m

## covert the matrix back into a vector (note the order of the values)
c(m)

## 2D arrays using the array function which produces the same object as matrix( c(1,2,3,4,5,6), nrow=2, ncol=3)
m <- array(NA, dim=c(2,3))
m

m <- array( c(1,2,3,4,5,6), dim=c(2,3))
m

## 2D arrays using the array function which produces the same object as matrix( c(1,2,3,4,5,6), nrow=2, ncol=3, byrow = TRUE)
m <- array( c(1,2,3,4,5,6), dim=c(2,3))
m

## arrays with more than 3 dimensions
m3 <- array( c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18), dim=c(2,3,3))
m3

## covert the array back into a vector
c(m3)

## create a new matrix
mymat <- rbind(c(1,3,4), 5:3, c(100,20,90), 11:13)
mymat

## additional functions to learn about the structure of the object (i.e., is it a matrix)
dim(mymat)
nrow(mymat)
ncol(mymat)
dim(mymat)[2]



##########################################################################
## code adapted from Section 3.2 in Davies
##########################################################################

## create a new matrix A with numeric values
A <- matrix(c(0.3,4.5,55.3,91,0.1,105.5,-4.2,8.2,27.9),nrow=3,ncol=3)

## print A
A

## print out one element in matrix A at row 3, column 2
A[3,2]

## print out one column in matrix A at column 2
A[,2]

## print out one row in matrix A at row 1
A[1,]

## print out row 2 and row 3 in matrix A
A[2:3,]

A_star <- A[2:3,]
A_star
 
## print out column 3 and column 1 in matrix A
A[,c(3,1)]


## print out elements that exist only in row 2 and row 3 and column 3 and column 1 in matrix A
A[c(3,1),2:3]

## diag is a function that works with matrices
## it returns the diagnoal elements from the upper left to the bottom right of the matrix
## in other words it returns a vector of values from position (1,1), (2,2), ... , (n,n)
diag(x=A)


## as with vectors, we can include or excludes values from the matrix for viewing or analysis
## print A to screen with all rows and without the second column
A[,-2]

## print A to screen without the first row and with only the 3rd and 2nd column
A[-1,3:2]

## print A to screen without the first row and without the second column
A[-1,-2]

#
A[-1,-c(2,3)]

## make a new object B, which is equivalent to and a copy of A
B <- A
B

## let's replace the 2nd row in B with a different vector of numbers
B[2,] <- 1:3
B

## now let's replace the two values in B
B[c(1,3),2] <- 900
B

## reverse order (flip) the 3rd row
B[,3] <- B[3,]
B

## here we are noting two positions and replacing those values with two new values
B[c(1,3),c(1,3)] <- c(-7,7)
B

## replace additional elements
B[c(1,3),2:1] <- c(65,-65,88,-88)
B

## change the diagnoal elements of b
diag(x=B) <- rep(x=0,times=3)
B



##########################################################################
## code adapted from Section 3.3 in Davies
##########################################################################

## create a new matrix A
A <- rbind(c(2,5,2),c(6,1,4))
A

## transpose A, which means to flip and rotate a
t(A)

## take the transpose of the transpose of A which returns A
t(t(A))


## create adiagonal matrix with 3 rows and 3 columns and 1 in the diagnoal positions and 0 otherwise, this is an identity matrix
A <- diag(x=3)
A


## create a new matrix A and multiply it by a scalar
A <- rbind(c(2,5,2),c(6,1,4))
a <- 2
a*A


## create two new matrices and do elementwise subtration
A <- cbind(c(2,5,2),c(6,1,4))
A
B <- cbind(c(-2,3,6),c(8.1,8.2,-9.8))
B
A-B


## create two new matrices and determin their dimensions
A <- rbind(c(2,5,2),c(6,1,4))
dim(A)
B <- cbind(c(3,-1,1),c(-3,1,5))
dim(B)

## matrix multiply (we will talk about this more later in the semester)
A %*% B

## matrix multiply (we will talk about this more later in the semester)
B %*% A


## create a matrix
A <- matrix(data=c(3,4,1,2),nrow=2,ncol=2)
A

## find the inverse of the matrix
solve(A)

## this creates an identity matrix
A %*% solve(A)


