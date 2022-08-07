## R_Demo_Intro_forloop_vs_lapply.R
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
## in class coding of loops and lapply

1:10

lenght(1:10)


## for loop version
output <- c()
for(i in 1:10){
  
  local_var <- i*i ## the simple but core calculation of the iterative process
  output[i] <- local_var
  
}
output


## lapply version
output <- lapply(1:10, function(i){
  
  local_var <- i*i ## the simple but core calculation of the iterative process
  return(local_var)
})
unlist(output)




mat <- matrix(NA, nrow=10, ncol=5)
mat

for(i in 1:10){
  for(j in 1:5){
    local_var <- i*j
    mat[i,j] <- local_var
  }  
}
mat

mat_list <- lapply(1:10, function(i){
  
  local_vec <- c()
  for(j in 1:5){
    local_var <- i*j
    local_vec[j] <- local_var
  }  
  
 return(local_vec)
})

unlist(mat_list)
mat <- matrix(unlist(mat_list), nrow=10, ncol=5, byrow=T)
mat


mat_list <- lapply(1:10, function(i){
  local_out <- lapply(1:5, function(j){
    temp <- i*j
    return(temp)
  })  
  return(local_out)
})

mat_list
unlist(mat_list)
mat <- matrix(unlist(mat_list), nrow=10, ncol=5, byrow=T)
mat

