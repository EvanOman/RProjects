# Basic idea of boosting:
# 1. Take lots of possibly weak predictors
# 2. Weight them and add them up (kind of like bagging)
# 3. Get a stronger predictor

# More detail:
# 1. Start with a set of classifiers h_1,...,h_k
# 2. Create a classifier that combines classification functions: f(x) = sgn(sum_{t=1}^T alpha_t h_t (x))
#   - Goal is to minimize the error
#   - Iterative, slect one h at each step
#   - Calculate weights based on errors
#   - Upweight missed classifications and select next h


# Loading the wage data
rm(list = ls())
library(ISLR)
data(Wage)
library(ggplot2)
library(caret)
Wage <- subset(Wage, select = -c(logwage))
inTrain <- createDataPartition(y=Wage$wage,p=.7,list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

# Fitting the model
modFit <- train(wage ~ ., method="gbm", data=training, verbose=FALSE)
print(modFit)

# Plot the results
pdf("boostingPlot.pdf")
qplot(predict(modFit,testing),wage,data=testing)
dev.off()