## R_Demo_text_as_data_word_probabilities_and_STM.R
##########################################################################
## INSTRUCTOR: Christopher Fariss
## COURSE NAME: Advanced Computational Methods for Social Media and Textual Data (3B)
## University of Essex Summer School 2023
##
## Date: 2023-08-13
##
## Please e-mail me if you find any errors or have and suggestions (either email is fine)
## e-mail: cjf0006@gmail.com
## e-mail: cjfariss@umich.edu
##########################################################################
## Introduction to tutorial:
##
## This tutorial demonstrates how the Structural Topic Model functions for a very simple set of documents.
##
## The documents are simulated and contain even and odd numbers between 0 and 99.
##
## The simulation draws from a probability distribution for each word.
##
## We can therefore see with certainty the probability that a document is of even or odd type (i.e., the topic) because it is the proportion of those words of a specific type that it contains.
##
## The topic proportions that the STM function returns are identical to the proportions we set through the simulation.
##
## We could complicate this tutorial by adding more words of different topics or characters that act as noise.
##
##########################################################################


## load libraries
library(stm)
library(tm)
library(MCMCpack)
library(MASS)
library(quanteda)
library(gtools)

## probability distribution for words (100 unique words 0:99)
word_probs <- rbeta(100,1,100)
summary(word_probs)

## graph word probabilities
par(mfrow=c(2,2), mar=c(4,4,1,1))
truehist(word_probs)

## set the total number words across all the documents in the corpus
total_words <- 100000

## categorical sampling from normalized word distribution
total_corpus <- sample(0:99, size=total_words, replace=TRUE, prob=word_probs)

table(total_corpus)

## plot the corpus size
barplot(table(total_corpus)[order(table(total_corpus), decreasing=T)]/total_words, space=0)

## checks
length(total_corpus)
length(table(total_corpus))
table(total_corpus)

## make functions to text if the word is "even" or "odd"
is.even <- function(x) x %% 2 == 0
is.odd <- function(x) x %% 2 != 0

## split corpus into even and odd parts
even_corpus <- total_corpus[is.even(total_corpus)]
length(even_corpus)
odd_corpus <- total_corpus[is.odd(total_corpus)]
length(odd_corpus)

table(odd_corpus)
table(even_corpus)


## create 1000 simulated documents by randomly assigning words to each of the documents
docs <- list()
for(i in 1:1000){
    
    doc_id <- i
    
    evens <- sample(even_corpus, size=sample(100:1000,size=1))
    odds <- sample(odd_corpus, size=sample(100:1000,size=1))
    
    length_evens <- length(evens)
    length_odds <- length(odds)
    total_length <- length_evens + length_odds
    prop_evens <- length_evens/(length_evens + length_odds)
    prop_odds <- length_odds/(length_evens + length_odds)
    
    text <- paste(as.character(c(evens,odds)), collapse=" ")
    
    docs[[i]] <- data.frame(doc_id, length_evens, length_odds, total_length, prop_evens, prop_odds, text)
}

## combine list elements into dataframe
docs_data <- do.call("rbind", docs)

dim(docs_data)

docs_data[1:10,1:6]

table(apply(docs_data[,5:6],1,sum))

docs_data[1,7]

is.character(docs_data[,7])

## create 1000 simulated documents by randomly assigning words to each of the documents
docs <- lapply(1:1000, function(i){

    doc_id <- i

    evens <- sample(even_corpus, size=sample(100:1000,size=1))
    odds <- sample(odd_corpus, size=sample(100:1000,size=1))

    length_evens <- length(evens)
    length_odds <- length(odds)
    total_length <- length_evens + length_odds
    prop_evens <- length_evens/(length_evens + length_odds)
    prop_odds <- length_odds/(length_evens + length_odds)

    text <- paste(as.character(c(evens,odds)), collapse=" ")
    
    docs <- data.frame(doc_id, length_evens, length_odds, total_length, prop_evens, prop_odds, text)

    return(docs)
})

## combine list elements into dataframe
docs_data <- do.call("rbind", docs)

## set and test that the text column is of type character
docs_data$text <- as.character(docs_data$text)
is.character(docs_data$text)

## summarize the dataframe
summary(docs_data[,1:6])

## make a DTM using the dfm function from the quanteda library
DTM <- dfm(docs_data$text)
dim(DTM)

DTM

## fit a structural topic model with two topics
fit <- stm(documents=DTM, K=2)


## check the attributes
attributes(fit)


## inspect the topic proportion which is theta
head(fit$theta, 10)

table(apply(fit$theta, 1, sum))

summary(fit$theta)

## make some more graphs
plot(fit$theta[,1], docs_data$prop_odds)
plot(fit$theta[,1], docs_data$prop_evens)

plot(fit$theta[,2], docs_data$prop_odds)
plot(fit$theta[,2], docs_data$prop_evens)

plot(fit$theta[,3], docs_data$prop_odds)
plot(fit$theta[,3], docs_data$prop_evens)

plot(fit$theta[,1], docs_data$prop_odds)
plot(fit$theta[,2], docs_data$prop_evens)

plot(fit$theta[,1], docs_data$prop_evens)
plot(fit$theta[,2], docs_data$prop_odds)

cor(fit$theta[,1], docs_data$prop_odds)
cor(fit$theta[,2], docs_data$prop_evens)

cor(fit$theta[,2], docs_data$prop_odds)
cor(fit$theta[,1], docs_data$prop_evens)


## another simulation this time using the Dirichlet density function
K <- 2   # num of topics
M <- 1000  # num of documents
V <- 100  # num of words

## set hyper parameters
hyper_parmameter_alpha <- rep(0.8, K)
hyper_parmameter_alpha

hyper_parmameter_beta <- rep(0.2, V)
hyper_parmameter_beta

## other simulation using the rdirichlet from the gtools library
## theta represents the distribution of topic probabilities for each document
theta <- rdirichlet(M, hyper_parmameter_alpha)

head(theta)

## the probabilities for each document (row) across all topics (column) sum to 1
table(apply(theta,1,sum))

## phi represents the distibution of probabilities for each word in a topic
phi <- rdirichlet(K, hyper_parmameter_beta)

head(phi)

## the probabilities for each topic (row) across all words (column) sum to 1
apply(theta,1,sum)

## the probabilities for each word (column) across all topics (rows) sum to 1
apply(phi,1,sum)


## estimate the above simulation using the STM package

## try a Stan model
data_list <- list(
    K = K,
    M = M,
    V = V,
    N = N,
    W = docs_data$doc_id,
    Offset = offset,
    hyper_parmameter_alpha = rep(1, K), ##
    hyper_parmameter_beta = rep(0.5, V) ##
)

