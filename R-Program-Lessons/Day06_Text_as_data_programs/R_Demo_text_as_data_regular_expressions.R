## R_Demo_text_as_data_regular_expressions.R
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
##
## Introduction to tutorial:
##
## (1) pattern matching with regular expression functions (see Chapter 11):
## grep()
## grepl()
## sub()
## gsub()
## regexpr()
## gregexpr()
##
## (2) Begin to organize a vector of text into a matrix that records the frequency of all the unique works in the full vector of text. This is called a document-by-term-matrix or DTM for short.
##
##########################################################################

## g is for get

## grepl() is logical function that returns TRUE if the first character argument is contained the second character argument
grepl("a", c("abc", "def"))
grepl("b", "abc")
grepl("c", "abc")
grepl("d", "abc")

## grep() function that returns the coordinate position of the vector in the second character argumnet
grep("a", c("a", "b", "c", "d", "e", "f", "g"))
grep("b", c("a", "b", "c", "d", "e", "f", "g"))
grep("c", c("a", "b", "c", "d", "e", "f", "g"))
grep("d", c("a", "b", "c", "d", "e", "f", "g"))

## sub() and gsub() look for a character string called the pattern in a vector of characters (third argument x) and replaced it was the character string in the second argument
gsub(pattern="a", "x", c("abc", "def"))
gsub(pattern="b", "", "abc")
gsub(pattern="c", "", "abc")

## let's look at the difference between sub() and gsub()
sub(pattern="a", "", "abc")
sub(pattern="b", "", "abc")
sub(pattern="c", "", "abc")

## gsub() replaces ALL instances of the pattern
gsub(pattern="a", "", "abcabc")
gsub(pattern="b", "", "abcabc")
gsub(pattern="c", "", "abcabc")

## sub() only replaces the FIRST instance of the pattern
sub(pattern="a", "", "abcabc")
sub(pattern="b", "", "abcabc")
sub(pattern="c", "", "abcabc")

## gregexp()

## read in fake tweeter data that I made up
readLines("Datasets/SIMpoliticalTweets.txt")[1]
tweets <- readLines("Datasets/SIMpoliticalTweets.txt", n=-1)

## print the tweets to screen
tweets

## which coordinates of the vector of tweets contains the term "obama" (note that we are assuming everything is lower case for now)
grep("obama", tweets)

## logical vector
grepl("obama", tweets)

## logical vector as binary data
as.numeric(grepl("obama", tweets))

## create binary vectors where 1 indicates if the term was present in the tweet and 0 otherwise
obama <- as.numeric(grepl("obama", tweets))
love <- as.numeric(grepl("love", tweets))
hate <- as.numeric(grepl("hate", tweets))

obama
love
hate

## tabulate the occurance of obama and one of the two emotional expressions
table(obama,love)
table(obama,hate)


## look at the output as columns
cbind(obama,love,hate)

## create a data frame
tweet_data <- as.data.frame(cbind(obama,love,hate))
tweet_data

## write the data frame as a csv file
write.csv(tweet_data, "Datasets/tweet_data.csv")

## save the tweet data as an R object
save(tweet_data, file="Datasets/tweet_data.Rdata")

## the tweet_data object is a document by term dataset made up of a subset of the unique terms that are in the original data file we loaded into R
tweet_data

## let's make a DTM (document-by-term-matrix) will all the unique terms

## read in fake tweeter data that I made up
tweets <- readLines("Datasets/SIMpoliticalTweets.txt", n=-1)

## print the tweets to screen
tweets

## strsplit() is a function that takes a character vector and splits it into a vector whenever it finds a specific character. We will take advantage of the fact that english used the space " " to denote when one words ends and a new word begins.

## slits every tweet into a vector of letters contained in a list (each element of the list is the original tweet)
strsplit(tweets, "")

## splits every tweet into a vector or words contained in a list (each element of the list is the original tweet)
strsplit(tweets, " ")

## make a list of vectors with each word in the tweet as an element of the vector
all_terms_list <- strsplit(tweets, " ")
all_terms_list

## make one long vector all the words
all_terms <- unlist(all_terms_list)
all_terms

## look at the frequency of the words:
table(all_terms)

## take the unique terms
unique_terms <- unique(all_terms)
unique_terms

## or put the tabulation into a data.frame() to get BOTH the unique terms and the frequency of the terms
data.frame(table(all_terms))

## let's do this for every element of our all_terms_list with a for loop



