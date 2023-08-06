## R_Demo_google_trends_Human_Rights.R
#########################################################################
## INSTRUCTOR: Christopher Fariss
## COURSE NAME: Advanced Computational Methods for Social Media and Textual Data (2F)
## University of Essex Summer School 2022
##
## Date: 2022-08-28
##
## Please e-mail me if you find any errors or have and suggestions
## e-mail: cjf0006@gmail.com
## e-mail: cjfariss@umich.edu
##########################################################################
## Introduction to tutorial:
rm(list=ls())

## load libraries
library(gtrendsR)
library(countrycode)
library(stm)
library(tm)
library(MASS)
library(colorbrewer)
library(bcp)


COLORS <- c("#fdae61", "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c")


#pdf("Texas_google_trends.pdf", height=5, width=6)

#plot.new()

TERMS <- list(c("%2Fg%2F11f06hfpld", "%2Fg%2F11nycqk4xh"),
              c("%2Fg%2F11f06hfpld", "%2Fm%2F03m1n"),
              c("%2Fm%2F03m1n", "%2Fg%2F11nycqk4xh"),
              c("%2Fg%2F11nycqk4xh", "%2Fm%2F01rbhn"),
              c("%2Fg%2F11f06hfpld", "%2Fm%2F01rbhn"),
              c("%2Fm%2F03m1n", "%2Fm%2F01rbhn")
)
length(TERMS)

TERMS_names <- list(c("Hurricane Harvey", "Winter Storm"),
                    c("Hurricane Harvey", "Astros"),
                    c("Astros", "Winter Storm"),
                    c("Winter Storm", "Power Outage"),
                    c("Hurricane Harvey", "Power Outage"),
                    c("Astros", "Power Outage")
)

#par(mfrow=c(3,3), mar=c(2,2.5,1,.5))
par(mfrow=c(1,1), mar=c(2,2.5,1,.5))

for(i in 1:6){
  
  world <- gtrends(TERMS[[i]], geo="US-TX", time="2017-01-01 2021-12-31", low_search_volume=T)$interest_over_time
  
  world$hits[world$hits=="<1"] <- .5
  world$hits <- as.numeric(world$hits)
  
  plot(world$hits[world$keyword==TERMS[[i]][1]], main=paste("Texas:", TERMS_names[[i]][1], "vs.", TERMS_names[[i]][2]), lwd=1, col=grey(.75), ylim=c(0, 100), xaxt="n", yaxt="n", type="n")
  #lines(world$hits[world$keyword==TERMS[[i]][2]], type="l", ylim=c(0,100), col=2)
  #plot.window(xlim=c(1,length(world$hits[world$keyword==TERMS[[i]][1]])), ylim=c(0, 100))
  #mtext(side=3, text=paste("Global:", TERMS[[i]][1], "vs.", TERMS[[i]][2]), line=1)
  
  id2017 <- which(as.Date(world$date[world$keyword==TERMS[[i]][1]]) > as.Date("2016-12-31") & as.Date(world$date[world$keyword==TERMS[[i]][1]]) <= as.Date("2017-12-31"))
  id2018 <- which(as.Date(world$date[world$keyword==TERMS[[i]][1]]) > as.Date("2017-12-31") & as.Date(world$date[world$keyword==TERMS[[i]][1]]) <= as.Date("2018-12-31"))
  id2019 <- which(as.Date(world$date[world$keyword==TERMS[[i]][1]]) > as.Date("2018-12-31") & as.Date(world$date[world$keyword==TERMS[[i]][1]]) <= as.Date("2019-12-31"))
  id2020 <- which(as.Date(world$date[world$keyword==TERMS[[i]][1]]) > as.Date("2019-12-31") & as.Date(world$date[world$keyword==TERMS[[i]][1]]) <= as.Date("2020-12-31"))
  id2021 <- which(as.Date(world$date[world$keyword==TERMS[[i]][1]]) > as.Date("2020-12-31") & as.Date(world$date[world$keyword==TERMS[[i]][1]]) <= as.Date("2021-12-31"))
  
  polygon(x=c(min(id2017), min(id2017), max(id2017), max(id2017)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
  polygon(x=c(min(id2019), min(id2019), max(id2019), max(id2019)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
  polygon(x=c(min(id2021), min(id2021), max(id2021), max(id2021)), y=c(-10,110,110,-10), col=grey(.95), border=NA)
  box()
  
  lines(world$hits[world$keyword==TERMS[[i]][1]], lwd=1, col="#c2a5cf")
  lines(world$hits[world$keyword==TERMS[[i]][2]], lwd=1, col="#a6dba0")
  axis(side=2, at=c(0,25,50,75,100), las=2)
  axis(side=1, at=c(median(id2017),  median(id2018), median(id2019), median(id2020), median(id2021)), labels=c(2017:2021), las=1)
  model <- bcp(y=world$hits[world$keyword==TERMS[[i]][1]])
  lines(model$posterior.mean, lwd=.75, col="#7b3294")
  model <- bcp(y=world$hits[world$keyword==TERMS[[i]][2]])
  lines(model$posterior.mean, lwd=.75, col="#008837")
  
  legend("topleft", legend=c(TERMS_names[[i]][1]), text.col="#7b3294" ,bty="n")
  legend("bottomleft", legend=c(TERMS_names[[i]][2]), text.col="#008837", bty="n")
  
}

#dev.off()
