## R_intro_loops_logic_lists_dataframes_tables.R
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
## (1) learn to work with logical statements
## (2) learn to use for loops and while loops
## (3) navigate vectors, matrices, lists, dataframes, and tables using logical statements and loops
## (4) apply functions to matrices, lists, dataframes, and tables
##
##########################################################################

## logical statements return TRUE or FALSE but never both for any statement or compound statement

###########
## EQUAL ##
###########

## these statements are setup as potentially true or false. we are asking R to evaluate whether or not they are true or false

## == tests if the left-hand side of the double-equal sign is the same as the value on the right-hand side of the double-equal sign

## the statements below test the truth status of statements set up to be true
1 == 1

pi == pi

1 == pi

1 == 2


###############
## NOT EQUAL ##
###############

## != tests if the left-hand side of the not-equal sign is NOT the same as the value on the right-hand side of the not-equal sign

## we can also ask for the truth status of statements set up to be false
1 != 1

pi != pi

1 != pi

1 != 2


## so far we have looked at simple statements. we can make these compond statments using the AND operator & (also &&), the OR operator | (also ||) (there are more but let's not worry about these additional operators now)

#########
## AND ##
#########

1 == pi & pi == pi

## the above statement returns an equivalent to:
FALSE & TRUE

## for an AND statement to return TRUE, both the left-hand side and right-hand side of the logical statement must both be TRUE conditions themselves

1 == 1 & pi == pi

TRUE & TRUE

########
## OR ##
########

## the OR operator looks for at least one condition in a compond set of conditions to be true
## the following statement return TRUE because at least one of the component conditions is TRUE
1 == pi | pi == pi

FALSE | TRUE

## if both statements are true then OR returns TRUE
1 == 1 | pi == pi

TRUE | TRUE

## if both statements are false then OR returns FALSE
1 == pi | 2 == pi

FALSE | FALSE


#########
## XOR ##
#########

## the xor() function is a function for XOR, in which not both conditions must be true  for XOR to return TRUE (we won't use this much if ever)

## this statement is FALSE because both statements are TRUE
xor(1 == 1, pi == pi)

## this statement is TRUE because one statement is TRUE and one statement is FALSE
xor(1 == 1, pi == 1)

xor(1 == pi, 1 == 1)

## this statement is FALSE because both statements are FALSE
xor(1 == pi, pi == 1)


##########################################################################
## if, else, and ifelse
##########################################################################

## inside an if statement we will use && and || instead of & and |

if(TRUE){ print("print this character string") }

if(TRUE | FALSE) print("print this character string")

if(1 == 1 & pi == pi){
    print("print this character string")
}

if(1 == pi & pi == pi){
    print("print this character string")
}

## check the condition in the if statement, if that is TRUE print, else print the second statement
## note that the else statement needs to be on the same line as the right sqiggly bracket }
if(1 == pi & pi == pi){
    print("print this character string")
} else{
    print("this is another character string!!")
}


## ifelse() takes three arguments, a logical condition and two values to be returned for either the TRUE or FALSE condtion

ifelse(pi==1, pi, 1)

ifelse(1 == pi & pi == pi, "print this character string", "is this really a helpful example??")




##########################################################################
## > greater than
## < less than
## >= greater than or equal to
## <= less than or equal to
##########################################################################

## 1 is greater than pi is FALSE
1 > pi

## pi is less than 1 is FALSE
pi < 1


## 1 is greater than 1 is TRUE
pi > 1

## create a vector of logical values for each condition (there will be one FALSE condition)
1:5 > 1

## create a vector of logical values for each condition (there will be 0 FALSE conditions)
1:5 >= 1



##########################################################################
## navigating objects with logical statements instead of coordinates
##########################################################################

## read data from the current working directory
social_media_data <- read.csv("users-by-social-media-platform.csv", header=TRUE)

## print only values for facebook using the subset function
subset(social_media_data, Entity=="Facebook")

## print only values for facebook using the subset function
subset(social_media_data, Entity=="Facebook" & Year>=2016)

## print only values for entities that are NOT facebook using the subset function
subset(social_media_data, Entity!="Facebook" & Year>=2016)




##########################################################################
## loops
##########################################################################

## Note: see Matloff Chapter 7 (Control Structures) for more information on the material below



##########################################################################
## for loops
##########################################################################

## the for loop is constructed so that it iterates for the length of vector, while keeping track of the indicator variable

index <- rnorm(10)
index
for(i in index){
    print(i)
}


## print the indicator variable each time the loop iterates
for(i in 1:10){
    print(i)
}


## print the indicator variable times the indicator variable each time the loop iterates
for(i in 1:10){
    print(i*i)
}

## print the indicator variable minus the indicator variable each time the loop iterates
for(i in 1:10){
    print(i-i)
}


## declare the object in memory so we can access it later on
vec <- c()
vec

## set i to the ith position of the vector each ith iteration the loop
for(i in 1:10){
    vec[i] <- i ## or something more complicated
}
vec


vec <- c()
for(i in 1:10){
    vec[i] <- i*i
}
vec


## we can declared a variable and add 1 to it each iteration of the loop
count <- 0
for(i in 1:10){
    count <- count + 1
}
count
i


##########################################################################
## while loop
##########################################################################

## the while loop is constructed to do something as long as a logical condition is TRUE
i <- 0

## is starts at 0
i

## this structure is similar to the for loop in terms of the process; however it ends when a condition is met instead of iterating through the values of an index
while(i <= 10){
    i <- i + 1
}

## i is 11 because the loop stopped once i no longer satisfied the logical condition
i



##########################################################################
## using loops to travel through the coordinates of a matrix or vector
##########################################################################

## create a matrix mat
mat <- matrix(1:12, nrow=4, ncol=3)
mat


## we can explore the matrix using the loop and our knowledge of the coordinates of each positions in the 2-D space

for(i in 1:nrow(mat)){
    print(mat[i,])
}


## we can also nest the two loops together to explore 2 dimensions of the matrix
for(i in 1:nrow(mat)){
    for(j in 1:ncol(mat)){
        print(mat[i,j])
    }
}

## we can switch the order by which we proceed through the coordinates of the matrix
for(j in 1:ncol(mat)){
    for(i in 1:nrow(mat)){
        print(mat[i,j])
    }
}

## let's use the single for loop to calculate values for each row in the matrix

## declare an empty object in memory so that we can store values in later with reference to its coordinates
row_sums <- c()

for(i in 1:nrow(mat)){
    row_sums[i] <- sum(mat[i,])
}
row_sums


## let's repeat this task for columns
column_sums <- c()

for(j in 1:ncol(mat)){
    column_sums[j] <- sum(mat[,j])
}
column_sums



##########################################################################
## apply() a function to margins (the rows or columns) of an array or matrix
##########################################################################

## create a matrix mat
mat <- matrix(1:12, nrow=4, ncol=3)
mat

## use the sum function on the rows (the first dimension of the matrix)
apply(mat, MARGIN=1, FUN=sum)

## use the sum function on the columns (the second dimension of the matrix)
apply(mat,2,sum)


## use the mean function on the rows (the first dimension of the matrix)
apply(mat,1,mean)

## use the mean function on the columns (the second dimension of the matrix)
apply(mat,2,mean)

## we can pass any function that works on numbers through the apply function
## the function is applied to every value in the row or column



##########################################################################
## tapply() a function to margins (the rows or columns) of a dataframe
##########################################################################

## NOTE: In my applied work, I find tapply() to be a little bit confusing and I have typically been able to create group level summaries using other tools (see above)

## use the cross-tabs function to count the number of values contained in each Entity by Year combination
tabs <- xtabs(monthly_active_users ~ Entity + Year, data= social_media_data)

## tabs is a table
tabs

## we can take the average across the rows of the table using tapply()

## we need to first create an object of the same size as the table with values for which groups we want to apply our funtion to
coordinate_groups <- matrix(rep(1:19,times=18), nrow=19, ncol=18)
coordinate_groups
tapply(tabs, INDEX=coordinate_groups, FUN=mean)

coordinate_groups <- matrix(rep(1:18,each=19), nrow=19, ncol=18)
coordinate_groups
tapply(tabs, INDEX=coordinate_groups, FUN=mean)


coordinate_groups_6_years <- cbind(matrix(1, nrow=19, ncol=6),
                           matrix(2, nrow=19, ncol=6),
                           matrix(3, nrow=19, ncol=6))
coordinate_groups_6_years
tapply(tabs, INDEX=coordinate_groups_6_years, FUN=sum)

coordinate_groups_by_decade <- cbind(matrix(1, nrow=19, ncol=9),
                           matrix(2, nrow=19, ncol=9))

coordinate_groups_by_decade
tapply(tabs, INDEX=coordinate_groups_by_decade, FUN=sum)


coordinate_groups_squares <- rbind(
                                cbind(matrix(1, nrow=10, ncol=9),
                                     matrix(2, nrow=10, ncol=9)),
                                cbind(matrix(3, nrow=9, ncol=9),
                                     matrix(4, nrow=9, ncol=9)
                                         )
                            )

coordinate_groups_squares
tapply(tabs, INDEX=coordinate_groups_squares, FUN=sum)



##########################################################################
## lapply() a function to each element in a list
##########################################################################

new_list <- list(1:5, 1:10)
new_list

lapply(new_list, sum)

## lapply() is one of the most useful and most confusing functions in base R
## lapply() is essential for large scale data manipulation and simulations when using base R
## but it is confusing!
## I promise it will make sense by the end of the course

## there are number of ways to set up the lapply() function
## my advice for learning to use the lapply() function is to make it "look like a for loop"

## simple for loop
vec <- c()
vec

for(i in 1:5){
    vec[i] <- i*i
}
vec

## re-write the for loop as a function applied to an empty list
## the content of the function should be identical to that of the for loop (note that we have to specify the specific object we want the function to return
#list()

vec <- lapply(1:5, function(i){
    i*i ## anything that goes into a for loop
})
vec

unlist(vec)


vec <- lapply(1:5, function(i){
    local_variable <- i*i ## anything that goes into a for loop
    return(local_variable)
})
vec
unlist(vec)


## this is a powerful reformulation of the for loop but we can think about the two structures in exactly the same way


## let's do something computationally difficult with a for loop (don't worry about the guts of the function for now)
start_time <- Sys.time()
Sys.time() - start_time

start_time <- Sys.time()
simulation_values <- NA
for(i in 1:10000){
    simulation_values[i] <- sum(rnorm(100000))
}
summary(simulation_values)

## calculate the elapsed time
Sys.time() - start_time


## let's use the lapply version of the computationally difficult process
start_time <- Sys.time()

simulation_values_list <- lapply(1:10000, function(i){
    return(sum(rnorm(100000)))
})
summary(unlist(simulation_values_list))

## calculate the elapsed time
Sys.time() - start_time

## let's graph the results for comparison
library(MASS)
par(mfrow=c(1,2))
truehist(simulation_values, col="darkorange")
truehist(unlist(simulation_values_list), col="steelblue1")




##########################################################################
## compare apply, for loop, and vectorization (apply is also a form of vectorization)
##########################################################################

## declare variables

## to raise the number of simulations
#power <- c(2,3,4,5,6)
power <- c(2,3,4,5)
power

## don't go above 6!
#power <- c(2,3,4,5,6)

## calculate the time taken for each operation
simple_time <- NA
apply_time <- NA
loop_time <- NA

## loop through each operation and store the time taken in the variables declared above
for(i in 1:length(power)){
    ## print what power the loop is at
    print(power[i])
    
    ## set the number of draws to take
    n <- 10^power[i]
    
    ## generate 5 0,1 vectors of length n
    v1 <- rbinom(n, 1, 0.5)
    v2 <- rbinom(n, 1, 0.5)
    v3 <- rbinom(n, 1, 0.5)
    v4 <- rbinom(n, 1, 0.5)
    v5 <- rbinom(n, 1, 0.5)
    
    ## column bind the vectors together
    v <- cbind(v1, v2, v3, v4, v5)
    
    ## declare a vector that uses one of the sequence function to store numbers from 1 until n
    j <- 1:n
    
    ## create a variable to store the output
    out <- NA
    d1 <- Sys.time()
    out[j] <- v[j,1]+v[j,2]+v[j,3]+v[j,4]+v[j,5]
    simple_time[i] <- Sys.time()-d1
    
    ## print the summary while the loop is running to compare the output
    ## and make sure each procedure is generating the same answer
    print(summary(out))
    
    ## same as above but use the apply function
    d1 <- Sys.time()
    apply_out <- apply(v, MARGIN=1, FUN=sum)
    apply_time[i] <- as.list(Sys.time()-d1)
    
    print(summary(apply_out))
    
    
    ## same as above but use a for loop
    d1 <- Sys.time()
    loop_out <- NA
    for(k in 1:n){
        loop_out[j] <- sum(v[k,])
    }
    loop_time[i] <- Sys.time()-d1
    print(summary(loop_out))
    
}

## print the results to the screen
cbind(power, simple_time, apply_time, loop_time)


