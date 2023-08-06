## R_Demo_text_as_data_DTM.R
##########################################################################
## INSTRUCTOR: Christopher Fariss
## COURSE NAME: Advanced Computational Methods for Social Media and Textual Data (2F)
## University of Essex Summer School 2022
##
## Date: 2022-08-08
##
## Please e-mail me if you find any errors or have and suggestions (either email is fine)
## e-mail: cjf0006@gmail.com
## e-mail: cjfariss@umich.edu
##########################################################################
##
## Introduction to tutorial:
##
## Text as data is represented as a high-dimensional, sparse feature vector where the elements of the vector are the frequencies of specific words and phrases in the document.
##
## For the DTM, we let i = 1, ..., N index documents and w = 1, ..., W index the unique terms in the collection of documents.
##
## For each of the i documents, we determine the frequency of each of the unique $w$ words.
##
## Each of the D_iw entries in a DTM represents the number of times the w-th word appears in the i-th document.
##
##########################################################################

## load data
data <- read.csv("SIMpoliticalTweets.txt", header=FALSE)
data
names(data) <- c("text")
data

#trumptweets <- fromJSON("trump_json_files_20190707.txt")
#data <- trumptweets

## check first 6 rows of data
head(data)

## load stop words to remove from text (these are very common words)
stopwords <- read.delim("stopwords_twitter.txt", header=FALSE)
head(stopwords)


## define function
textEdit <- function(text.vector){
    TEXT <- text.vector
    ## ----- remove stuff ---------- #
    TEXT <- gsub("http:.*$", "", TEXT) # replace just the urls/http/www part
    TEXT <- gsub("https:.*$", "", TEXT) # replace just the urls/http/www part
    TEXT <- gsub("www.*$", "", TEXT) #
    TEXT <- gsub("http:.*", "", TEXT) # replace all of the urls
    TEXT <- gsub("https:.*", "", TEXT) # replace all of the urls
    TEXT <- gsub("www.*", "", TEXT) #
    TEXT <- gsub("[[:punct:]]", "", TEXT) # remove all punctuation
    TEXT <- gsub("[^\x20-\x7F\x0D\x0A]", "", TEXT) # remove all non-ascii characters
    TEXT <- gsub("[0-9]", "", TEXT) # remove numbers
    TEXT <- gsub("^\\s+|\\s+$", "", TEXT) # remove extra leading and trailing whitespace
    TEXT <- tolower(TEXT) # turn all letters lower case
    ## ----- function return ---------- #
    return(TEXT)
} ## end function


## use textEdit function to process text
newtext <- textEdit(as.character(data$text))
head(newtext)


## create list object
## each element is a tweet
## within each element are each of terms used in the tweet
tweet.term.list <- strsplit(newtext, split=" ")
tweet.term.list
table(tweet.term.list[[1]])

table(tweet.term.list[[2]])

table(unlist(tweet.term.list))

## view logical subsetting command to remove stopwords from the first tweet
tweet.term.list[[1]] %in% as.character(stopwords$V1)

## remove stopwords with not ! symbol for first elemnet in list of words (! flips FALSE to TRUE and TRUE to FALSE)
tweet.term.list[[1]][! tweet.term.list[[1]] %in% as.character(stopwords$V1) ]


## loop through elements of loop and remove stop words from each tweet
for(i in 1:length(tweet.term.list)){
    tweet.term.list[[i]] <- tweet.term.list[[i]][! tweet.term.list[[i]] %in% as.character(stopwords$V1) ]
}
tweet.term.list


## instead of using a loop, use the lapply function to accomplish the same task as the for loop above
tweet.term.list <- lapply(1:length(tweet.term.list), function(i){
    local_value <- tweet.term.list[[i]][! tweet.term.list[[i]] %in% as.character(stopwords$V1) ]
    return(local_value)
})


## take the unique terms that remain in the full list of tweets after revmoing stop words
table(unlist(tweet.term.list))

unique.terms <- unique(unlist(tweet.term.list))
unique.terms

## print to screen the total length of the unique term vector
length(unique.terms)

## print to screen the first 6 terms from the unique.terms object
head(unique.terms)





## set time start variable
time1 <- Sys.time()
print(Sys.time() - time1)

## create temporary dataset to append one term dataset per tween
## each term dataset is appended to the one before creating a long data stucture
## the units or records in this dataset are tweet-terms
## each record is the document-term combination


i <- 1
data.frame(Doc=i, as.data.frame(table(tweet.term.list[[i]])))


data.list <- list()

## this for loop takes a long time
for(i in 1:length(tweet.term.list)){
    ## test to make sure there is at least one term in the tweet
    if(length(tweet.term.list[[i]])!=0){
        data.list[[i]] <- data.frame(Doc=i, as.data.frame(table(tweet.term.list[[i]])))
    } else{     ## if there are not terms in the tweet than return NA values
        data.list[[i]] <- data.frame(Doc=i, Var1=NA, Freq=NA)
    }
    
}
data.list

## create dataframe from the first dataframe in the data.list object
newdata <- data.list[[1]]
newdata

## row bind each additional dataset from data.list to the newdata object
for(i in 2:length(data.list)){
    newdata <- rbind(newdata, data.list[[i]])
}
newdata

# rbind the list of data.frames together (this line is equivalent line 148-152)
newdata <- do.call("rbind", data.list)
newdata

## create matric of documents by terms with xtabs() function
DTM <- xtabs(Freq ~ Doc + Var1, data=newdata)
DTM

## print dimensions of the DTM
dim(DTM)


## inspect the frequency of terms for 25 terms in the firt 5 tweets
#DTM[1:5,1:25]


## cacluate total calculation time
print(Sys.time() - time1)




# set time start variable
time1 <- Sys.time()
print(Sys.time() - time1)


# for each j document, get the vector of terms seperated by white space
# use the table() function to determine the frequency of each term
# transform the table of frequencies into a datafram with j document indicator

data.list <- lapply(1:length(tweet.term.list), function(i) {
    if(length(tweet.term.list[[i]])!=0){
        local_value <- data.frame(Doc=i, as.data.frame(table(tweet.term.list[[i]])))
    }
    else{
        local_value <- data.frame(Doc=i, Var1=NA, Freq=NA)
    }
    return(local_value)
})

# rbind the list of data.frames together (this line is equivalent line 140-142)
newdata <- do.call("rbind", data.list)

## create matric of documents by terms with xtabs() function
DTM <- xtabs(Freq ~ Doc + Var1, data=newdata)

## print dimensions of the DTM
dim(DTM)


## inspect the frequency of terms for 25 terms in the firt 5 tweets
DTM
#DTM[1:5,1:25]


## cacluate total calculation time
print(Sys.time() - time1)




## create a doc_by_doc matrix
DDM <- DTM %*% t(DTM)
DDM

## create a term_by_term matrix
TTM <- t(DTM) %*% DTM
TTM


## create the TTM with a loop instead of using matrix multiplication
row.names(DTM)

Var1=="fake"

for(i in 1:length(unique.terms)){
  
  row.sums(DTM$Var1=="fake")
}
