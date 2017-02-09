# Basic Idea for Model Based ML:
# 1. Assume that the data follow some probabilistic model
# 2. Use Baye's Theorem to identify optimal classifiers

# More details:
# 1. Our goal is to build a parametric model for conditional distribution P(Y=k|X=x)
# 2. A typical approach is to apply Bayes Theorem
# 3. Typically prior probabilities pi_k are set in advance
# 4. A common choice is a Guassian distribution
# 5. Estimate the parameters (mu_k,sigma_k^2) from the data
# 6. Classify to the class with the highest value of P(Y=k|X=x)

rm(list=ls())
data(iris)
library(ggplot2)

# Load the data
names(iris)
table(iris$Species)

inTrain <- createDataPartition(y=iris$Species,p=.7,list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training)
dim(testing)

# Fit the model
modlda <- train(Species~.,data=training,method="lda")
modnb <- train(Species~.,data=training,method="nb")
plda <- predict(modlda,testing)
pnb <- predict(modnb,testing)
table(plda,pnb)

# Plot the results
pdf("modelBasedPredictions.pdf")
equalPredictions <- (plda==pnb)
qplot(Petal.Width,Sepal.Width,color=equalPredictions,data=testing)
dev.off()

