## R_Demo_text_as_data_NYT_Text_Process_v03.R
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
##
## Introduction to tutorial:
##
## For this R tutorial we will read in text files that contain news stories from the New York Times for four years in the 1980s.
##
## The code use real expression functions and other base package functios to process the data and transform the data into a document by term matrix (STM).
##
## The program uses the organizational features of the text files to locate each story within the document, extra article information about each story, save each story as a seperate text file, and create a dataset of article level information, all in addition to the DTM.
##
##########################################################################


## load library
library(reshape)
library(MASS) # loaed library with truehist function

## set time start variable
time1 <- Sys.time()
print(Sys.time() - time1)


## ----- function define ---------- #
textEdit <- function(text.vector){
  TEXT <- text.vector  
## ----- remove stuff ---------- #
  TEXT <- gsub("http:.*$", "", TEXT) # replace just the urls/http/www part
  TEXT <- gsub("www.*$", "", TEXT) #
  TEXT <- gsub("http:.*", "", TEXT) # replace all of the urls
  TEXT <- gsub("www.*", "", TEXT) #
  TEXT <- tolower(TEXT) # turn all letters lower case
  TEXT <- gsub("[[:punct:]]", "", TEXT) # remove all punctuation
  TEXT <- gsub("[^\x20-\x7F\x0D\x0A]", "", TEXT) # remove all non-ascii characters
  TEXT <- gsub("[0-9]", "", TEXT) # remove numbers
  TEXT <- gsub("^\\s+|\\s+$", "", TEXT) # remove extra leading and trailing whitespace
## ----- function return ---------- #
  return(TEXT)
} ## end function


## ----- load data ---------- #
workingdirectory <- getwd()
setwd(paste(workingdirectory, "/NYT_Text_Articles/unProcessed_Files/", sep=""))

FILES <- list.files()

gettext <- function(source){
lapply(1:length(source), function(i) {
  readLines(source)
})
}
rawtext <- gettext(FILES)


rawtext <- rawtext[-1]
rawtext <- rawtext[-1]
rawtext <-  gsub("^\\s+|\\s+$", "", rawtext)
rawtext <- gsub("([0-9]){1,} of ([0-9]){1,} DOCUMENTS", "==================================================", rawtext)
rawtext <- c(rawtext[-1], rawtext[1])
rawtext <-  gsub("^$|^( +)$|[\t\n\r\f\v]+", NA, rawtext)
rawtext <- na.omit(rawtext)
rawtext <- c("==================================================", rawtext)

## ----- define objects ---------- ##
STRING <- "=================================================="
doc <- terms <- list()
temp <- NA
j <- 0
Headline <- NewsService <- Byline <- Month <- Day <- Year <- NA
data <- c(NA, NA, NA)
names(data) <- c("doc", "term", "freq")


## ----- process objects ---------- #
j<-1
for(i in 1:length(rawtext)){
  if(i>1 & rawtext[i]==STRING){
    doc[[j]] <- temp
    writeLines(doc[[j]], con=paste(workingdirectory, "/NYT_Text_Articles/Processed_Files/", j, ".txt", sep=""))
    terms[[j]] <- unlist(strsplit(textEdit(doc[[j]]), " "))
    newdata <- cbind(j, as.data.frame(table(terms[[j]])))
    names(newdata) <- c("doc", "term", "freq")
    data <- rbind(data, newdata)
    Headline[j] <- doc[[j]][5]
    NewsService[j] <- doc[[j]][1]
    Byline[j] <- unlist(strsplit(doc[[j]][6], "BYLINE: By "))[2]
    date <- unlist(strsplit(doc[[j]][4], ","))[1:2]
    Month[j] <- unlist(strsplit(date, " "))[1]
    Day[j] <- unlist(strsplit(date, " "))[2]
    Year[j] <- gsub(" ", "", date[2])
    temp <- NA
    j <- j + 1
  }
  temp <- c(temp, rawtext[i])
}


## ----- make document-by-term matrix ---------- ##
DTM <- xtabs(freq ~ doc + term, data)
write.csv(DTM, paste(workingdirectory, "/NYT_Text_Articles/DTM.csv", sep=""), row.names=F)

totalwords <- apply(DTM[,-1], 1, sum)

## ----- make meta data file ---------- ##
metadata <- as.data.frame(cbind(1:(j-1), totalwords, Headline, Year, Month, Day, NewsService, Byline))
names(metadata) <- c("doc", "totalwords", "Headline",  "Year", "Month", "Day", "NewsService", "Byline")
write.csv(metadata, paste(workingdirectory, "/NYT_Text_Articles/metadata.csv", sep=""), row.names=F)

## ----- reset working directory ---------- ##
setwd(workingdirectory)
