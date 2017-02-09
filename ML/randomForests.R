rm(list=ls())
data(iris,caret); library(ggplot2)
inTrain <- createDataPartition(y=iris$Species, p=.7, list = FALSE)

training <- iris[inTrain,]
testing <- iris[-inTrain,]

modFit <- train(Species~.,data=training,method="rf",prox=TRUE)
modFit

getTree(modFit$finalModel,k=2)

# Plots the "centers" of the predicted values
pdf("forestCenters.pdf")
irisP <- classCenter(training[,c(3,4)],training$Species, modFit$finalModel$prox)
irisP <- as.data.frame(irisP)
irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length, col=Species,data=training)
p + geom_point(aes(x=Petal.Width,y=Petal.Length,col=Species),size=5,shape=4,data=irisP)
dev.off()

# Predict new values
pred <- predict(modFit,testing)
testing$predRight <- pred == testing$Species
table(pred,testing$Species)


# Plot the predictions
pdf("predictions.pdf")
qplot(Petal.Width,Petal.Length,colour=predRight,data=testing,main="newdata Predictions")
dev.off()