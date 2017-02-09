rm(list = ls())

library(ElemStatLearn);
data(ozone,package = "ElemStatLearn")
ozone <- ozone[order(ozone$ozone),]
head(ozone)

## Predict temperature as a function of ozone

# Create 10 rows which are different random samples of the data set
ll <- matrix(NA, nrow=10,ncol=155)
for (i in 1:10)
{
  ss <- sample(1:dim(ozone)[1], replace=TRUE)
  ozone0 <- ozone[ss,]
  ozone0 <- ozone0[order(ozone0$ozone),]
  loess0 <- loess(temperature ~ ozone, data=ozone0,span=.2)
  ll[i,] <- predict(loess0, newdata = data.frame(ozone=1:155))
}

# Create a plot of the 10 different loess models fitted to the 10 different samples
# along with the averaged or bagged version
pdf("baggedPlot.pdf")
plot(ozone$ozone, ozone$temperature, pch=19,cex=.5, xlab = "Ozone", ylab = "Temperature", main = "Ozone vs. Temperature Models with Bagging")
for (i in 1:10){lines(1:155, ll[i,],col="grey",lwd=2)}
lines(1:155,apply(ll,2,mean),col="red",lwd=2)
dev.off()

# Now we will make a decision tree model with a custom bagging function
library(party)
predictors <- data.frame(ozone = ozone$ozone)
temperature <- ozone$temperature
treebag <- bag(predictors, temperature, B = 10,
               bagControl = bagControl(fit = ctreeBag$fit,
                                       predict = ctreeBag$pred,
                                       aggregate = ctreeBag$aggregate))

pdf("treeBagging.pdf")
plot(ozone$ozone,temperature,col='lightgrey',pch=19)
points(ozone$ozone,predict(treebag$fits[[1]]$fit,predictors),pch=19,col="red")
points(ozone$ozone, predict(treebag,predictors),pch=19,col="blue")
dev.off()

