## R_Demo_Ternary_Diriclet_visulization.R
##########################################################################
## INSTRUCTOR: Christopher Fariss
## COURSE NAME: Advanced Computational Methods for Social Media and Textual Data (3B)
## University of Essex Summer School 2023
##
## Date: 2023-08-13
##
## Please e-mail me if you find any errors or have and suggestions
## e-mail: cjf0006@gmail.com
## e-mail: cjfariss@umich.edu
##########################################################################
## Introduction to tutorial:
##
## (1) The Ternary plot shows the distributions of random draws from a Diriclet distribution
## (2) The draws are situated within a 3-dimensional simplex. The values of the draws are also coordiantes within the space of the 3-dimensional simplex. The values range from 0 to 1.
## (3) Any point within the space of the 3-dimensional simplex also must sum to 1. 
##
##########################################################################

library(Ternary)
library(MCMCpack)

value1 <- value2 <- value3 <- value4 <-  value5 <-  value6 <- matrix(NA, nrow=1000, ncol=3)

## generate 6 different 3D coordinate systems that exists within a simplex such that the coordinates for any point our proportions that always sum to 1
value1[,1:3] <- rdirichlet(1000, alpha=c(1,1,5))
value2[,1:3] <- rdirichlet(1000, alpha=c(1,5,1))
value3[,1:3] <- rdirichlet(1000, alpha=c(10,10,1))
value4[,1:3] <- rdirichlet(1000, alpha=c(1,1,1))
value5[,1:3] <- rdirichlet(1000, alpha=c(10,10,10))
value6[,1:3] <- rdirichlet(1000, alpha=c(100,100,100))

head(value1[,1:3])


rdirichlet(10, alpha=c(1,1,1,1))

## show coordinates within the simplex sum to 1
table(apply(value1[,1:3],1,sum))
table(apply(value2[,1:3],1,sum))
table(apply(value3[,1:3],1,sum))
table(apply(value4[,1:3],1,sum))
table(apply(value5[,1:3],1,sum))
table(apply(value6[,1:3],1,sum))

## test coordinates within the simplex sum to 1
summary(apply(value1[,1:3],1,sum))
table(apply(value1[,1:3],1,sum)==1)

## unicode for right arrow: "\u2192"
## unicode for left arrow: "\u2190"

par(mfrow=c(3,2), mar=c(1,1,1,1))
TernaryPlot(alab="Redder \u2192", blab="\u2190 Greener", clab="Bluer\u2192",
    point='right', lab.cex=0.8, grid.minor.lines = 0,
    grid.lty='solid', col=rgb(0.9, 0.9, 0.9), grid.col='white',
    axis.col=rgb(0.6, 0.6, 0.6), ticks.col=rgb(0.6, 0.6, 0.6),
    padding=0.08, main="simplex")
TernaryPoints(value1, col=grey(.75))

TernaryPlot(alab="Redder \u2192", blab="\u2190 Greener", clab="Bluer\u2192",
            point='right', lab.cex=0.8, grid.minor.lines = 0,
            grid.lty='solid', col=rgb(0.9, 0.9, 0.9), grid.col='white',
            axis.col=rgb(0.6, 0.6, 0.6), ticks.col=rgb(0.6, 0.6, 0.6),
            padding=0.08, main="simplex")
TernaryPoints(value1, col=grey(.75))

TernaryPlot(alab="Redder \u2192", blab="\u2190 Greener", clab="Bluer\u2192",
            point='right', lab.cex=0.8, grid.minor.lines = 0,
            grid.lty='solid', col=rgb(0.9, 0.9, 0.9), grid.col='white',
            axis.col=rgb(0.6, 0.6, 0.6), ticks.col=rgb(0.6, 0.6, 0.6),
            padding=0.08, main="simplex")
TernaryPoints(value3, col=grey(.75))

TernaryPlot(alab="Redder \u2192", blab="\u2190 Greener", clab="Bluer\u2192",
            point='right', lab.cex=0.8, grid.minor.lines = 0,
            grid.lty='solid', col=rgb(0.9, 0.9, 0.9), grid.col='white',
            axis.col=rgb(0.6, 0.6, 0.6), ticks.col=rgb(0.6, 0.6, 0.6),
            padding=0.08, main="simplex")
TernaryPoints(value4, col=grey(.75))

TernaryPlot(alab="Redder \u2192", blab="\u2190 Greener", clab="Bluer\u2192",
            point='right', lab.cex=0.8, grid.minor.lines = 0,
            grid.lty='solid', col=rgb(0.9, 0.9, 0.9), grid.col='white',
            axis.col=rgb(0.6, 0.6, 0.6), ticks.col=rgb(0.6, 0.6, 0.6),
            padding=0.08, main="simplex")
TernaryPoints(value5, col=grey(.75))

TernaryPlot(alab="Redder \u2192", blab="\u2190 Greener", clab="Bluer\u2192",
            point='right', lab.cex=0.8, grid.minor.lines = 0,
            grid.lty='solid', col=rgb(0.9, 0.9, 0.9), grid.col='white',
            axis.col=rgb(0.6, 0.6, 0.6), ticks.col=rgb(0.6, 0.6, 0.6),
            padding=0.08, main="simplex")
TernaryPoints(value6, col=grey(.75))

