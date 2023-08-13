## R_Demo_text_as_wikip_stm.R
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
## This tutorial demonstrates how the Structural Topic Model functions for a real set of documents.
##
## Each unit in the dataset representes a randomly selected sentence from wikipedia.  
##
##########################################################################

library(stm)

wikip_word_dat <- readLines("one_meelyun_sentences.txt")
head(wikip_word_dat)

n <- length(wikip_word_dat)
n

wikip_word_dat <- data.frame(id=1:n, text=wikip_word_dat)
head(wikip_word_dat)


wikip_word_dat$human_rights_test <- as.numeric(grepl("human rights", wikip_word_dat$text))
table(wikip_word_dat$human_rights_test)

wikip_word_dat$civil_rights_test <- as.numeric(grepl("civil rights", wikip_word_dat$text))
table(wikip_word_dat$civil_rights_test)

## preprocess the documents
## This function uses function from the tm package (see the tm Demo for more details)
prep <- textProcessor(documents=wikip_word_dat$text, meta=wikip_word_dat)

save(out, file="wikip_word_dat_stm_prep.Rdata")

## inspect
head(prep$documents)
head(prep$vocab)
head(prep$meta)


## pre Documents
## stem words and remove stop words
out <- prepDocuments(prep$documents, prep$vocab, prep$meta)

save(out, file="wikip_word_dat_stm_out.Rdata")

## inspect
head(out$documents)
head(out$vocab)
head(out$meta)

## fit a structural topic model
#fit <- stm(documents=out$documents, vocab=out$vocab, data=out$meta, K=20)
#fit <- stm(documents=out$documents, vocab=out$vocab, data=out$meta, K=40)
fit <- stm(documents=out$documents, vocab=out$vocab, data=out$meta, K=100)

#fit_2 <- fit
#fit_3 <- fit
#fit_10 <- fit
#fit_20 <- fit
fit_40 <- fit

## display topic probabilities
head(fit$theta)

dim(fit$theta)
summary(head(fit$theta, 1000))

#save(fit, file="wikip_word_dat_stm20.Rdata")
#save(fit, file="wikip_word_dat_stm40.Rdata")
save(fit, file="wikip_word_dat_stm100.Rdata")


