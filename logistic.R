logistic.map <- function(c, x, N, M){
    ## r: bifurcation parameter
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
my.r <- seq(-.25, -.01, by=0.00001)
system.time(Orbit <- sapply(my.r, logistic.map,  x=-0.001^(.25), N=600, M=100))
##   user  system elapsed (on a 2.4GHz Core2Duo)
##   2.910   0.018   2.919 

Orbit <- as.vector(Orbit)
r <- sort(rep(my.r, 101))

mypanelfn <- function(x,y,...)
{
    panel.xyplot(x=x, y=y, ...)
    grid.lines(c(0,1), unit(rep(.001^(.25), 2), "native")
}

crit = .001^(.25)

png(filename="./orbit.png",  width=2000, height=2000)
xyplot(Orbit ~ r, col = "black", pch = ".", cex = 1, ylim = c(-1,1), panel=function(...) {
    panel.abline(h=c(crit, - crit))
    panel.xyplot(...)
}, scales=list(
    x=list(
        at=seq(-.25,-.01, by=.01)
)))
dev.off()
