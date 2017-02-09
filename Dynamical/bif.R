library(parallel)
library(lattice)
logistic.map <- function(c, x, N, M){
    ## c: bifurcation parameter
    ## x: initial value
    ## N: number of iteration
    ## M: number of iteration points to be returned
    z <- 1:N
    z[1] <- x
    for(i in c(1:(N-1))){
        z[i+1] <- z[i]^2 + c + .001/z[i]^2
    }
    ## Return the last M iterations 
    z[c((N-M):N)]
}

## Set scanning range for bifurcation parameter r
my.c <- seq(-0.25, .025, length= 20000)
system.time(Orbit <- sapply(my.c, logistic.map,  x=.001^(1/4)
                            , N=500, M=100))


Orbit <- as.vector(Orbit)
c <- sort(rep(my.c, 101))

crit = .001^(.25)

png(filename="./pertzoom1.png",  width=1000, height=1000)
xyplot(Orbit ~ c, col = "black", pch = ".", cex = 1, ylim = c(-1.25,1.25), panel=function(...) {
    panel.abline(h=c(crit, - crit))
    panel.xyplot(...)
}#, scales=list(
#     x=list(
#         at=seq(-2, .25, by=0.1)
#))
)
dev.off()


