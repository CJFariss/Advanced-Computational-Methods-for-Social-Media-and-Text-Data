## R_Demo_Ternary_Diriclet.R
library(Ternary)
library(MCMCpack)

value1 <- value2 <- value3 <- value4 <-  value5 <-  value6 <- matrix(NA, nrow=1000, ncol=3)

value1[,1:3] <- rdirichlet(1000, alpha=c(1,1,5))
value2[,1:3] <- rdirichlet(1000, alpha=c(1,5,1))
value3[,1:3] <- rdirichlet(1000, alpha=c(10,10,1))

value4[,1:3] <- rdirichlet(1000, alpha=c(1,1,1))
value5[,1:3] <- rdirichlet(1000, alpha=c(10,10,10))
value6[,1:3] <- rdirichlet(1000, alpha=c(100,100,100))

## coordinates within the simplex sum to 1
table(apply(value4[,1:3],1,sum))


## unicode for right arrow: "\u2192"
## unicode for left arrow: "\u2190"

TernaryPlot(alab="Redder \u2192", blab="\u2190 Greener", clab="Bluer\u2192",
    point='right', lab.cex=0.8, grid.minor.lines = 0,
    grid.lty='solid', col=rgb(0.9, 0.9, 0.9), grid.col='white',
    axis.col=rgb(0.6, 0.6, 0.6), ticks.col=rgb(0.6, 0.6, 0.6),
    padding=0.08, main="simplex")
TernaryPoints(value1, col=4)
TernaryPoints(value2, col=3)
TernaryPoints(value3, col=2)

TernaryPoints(value4, col=1)
TernaryPoints(value5, col=4)
TernaryPoints(value6, col=2)
