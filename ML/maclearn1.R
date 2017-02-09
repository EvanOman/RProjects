data(iris); library(ggplot2); library(rpart);
names(iris);

table(iris$Species)

# split into training and test sets
inTrain <- createDataPartition(y=iris$Species, p=.7,list=FALSE)

training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training);
dim(testing)

qplot(Petal.Width, Sepal.Width, color=Species, data=training)

library(caret);
modFit <- train(Species~.,method="rpart",data=training)
print(modFit$finalModel)

library(rattle)
fancyRpartPlot(modFit$finalModel)


predict(modFit, newdata = testing)

