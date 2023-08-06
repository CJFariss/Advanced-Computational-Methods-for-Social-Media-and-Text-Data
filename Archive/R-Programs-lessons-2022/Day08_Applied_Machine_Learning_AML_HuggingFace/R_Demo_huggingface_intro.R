## R_Demo_huggingface_into.R
##########################################################################
## INSTRUCTOR: Christopher Fariss
## COURSE NAME: Advanced Computational Methods for Social Media and Textual Data (3B or 2F)
## University of Essex Summer School 2022
##
## Date: 2022-08-17
##
## Please e-mail me if you find any errors or have and suggestions (either email is fine)
## e-mail: cjf0006@gmail.com
## e-mail: cjfariss@umich.edu
##########################################################################
##
## Introduction to tutorial:
##
## We are going to try out a number of pretrained Large Langugae Models --- LLMs --- that are publicly available at 🤗
##
## 🤗 huggingface models: https://huggingface.co/models
## 
## 🤗 huggingface datasets: https://huggingface.co/datasets
##
## Bert LLM (and many others)
## https://huggingface.co/docs/transformers/v4.20.1/en/model_doc/bert
##
## BloomLLM 
## BigScience Large Open-science Open-access Multilingual Language Model
## https://huggingface.co/bigscience/bloom
##
## more examples with R
## https://rpubs.com/eR_ic/transfoRmers
##
## for loading Python libraries and setting up piplines (functions)
## https://rstudio.github.io/reticulate/reference/py_install.html
## for details see https://rstudio.github.io/reticulate/
##
##########################################################################
##
## install.packages("reticulate")

## load R libraries
library(reticulate)
library(keras)
library(tensorflow)
library(dplyr)
library(tfdatasets)
library(torch)

## load Python libraries using the reticulate package
reticulate::py_install("transformers", pip = TRUE)

## also try these
reticulate::py_install("PyTorch", pip = TRUE)
reticulate::py_install("tensorflow", pip = TRUE)

## or try this
#tensorflow::install_tensorflow()

# Importing 🤗 transformers module into R session using the Python-to-R reticulate package
transformers <- reticulate::import("transformers")

# get another pretrained Tokenizer
transformers$RobertaTokenizer$from_pretrained('roberta-base', do_lower_case=TRUE)

# Instantiate a Python pipeline (this is now a function that we can pass inputs into)
## more information on piplines here: https://huggingface.co/docs/transformers/main_classes/pipelines
## the text-classification pipeline is for "sentiment-analysis"
classifier <- transformers$pipeline(task = "text-classification")

## load the simple fake text data
data <- read.csv("SIMpoliticalTweets.txt", header=FALSE)
names(data)  <- "text"
head(data)

# Generate predictions using the Python pipline classifier() function 
outputs <- classifier(data$text)
outputs

outputs[[1]]$score

data.frame(outputs)



# get another pretrained Tokenizer
transformers$RobertaTokenizer$from_pretrained('roberta-base', do_lower_case=TRUE)

classifier <- transformers$pipeline(task = "text-classification")

outputs <- classifier(data$text)
outputs

# get Model with weights
transformers$TFRobertaModel$from_pretrained('roberta-base')



## try some others pre-trained tasks with the zero-shot-classification
## https://huggingface.co/docs/transformers/v4.21.1/en/main_classes/pipelines#transformers.ZeroShotClassificationPipeline
classifier <- transformers$pipeline(task = "zero-shot-classification")

outputs <- classifier(data$text, c("love", "obama"))
outputs


## another classifier example, this time translating an english term into French


transformers$TFRobertaModel$from_pretrained("t5-base")
en_fr_translator <- transformers$pipeline(task = "translation_en_to_fr")
en_fr_translator("human rights")

en_de_translator <- transformers$pipeline(task = "translation_en_to_de")
en_de_translator("human rights")


## another classifier example
token_classifier <- transformers$pipeline(task = "token-classification")

token_classifier()

