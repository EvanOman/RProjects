# Question 1
# set.seed(33833)
# mod1 <- train(y~., method="rf", data = vowel.train)
# mod2 <- train(y~., method="gbm", data = vowel.train)
# 
# pred1 <- predict(mod1, vowel.test)
# pred2 <- predict(mod2, vowel.test)
# 
# confusionMatrix(pred1, vowel.test$y)
# confusionMatrix(pred2, vowel.test$y)
# 
# agreedBool = pred1 == pred2
# 
# agreed = pred1[agreedBool]
# agreedTest = vowel.test[agreedBool, "y"]
# 
# confusionMatrix(agreed, agreedTest)

# Question 2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)

# Model building
rfMod <- train(diagnosis ~ ., method="rf", data = training)
gbmMod <- train(diagnosis ~ ., method="gbm", data = training)
ldaMod <- train(diagnosis ~ ., method="lda", data = training)

# Making predictions
rfPred <- predict(rfMod, newdata = training)
gbmPred <- predict(gbmMod, newdata = training)
ldaPred <- predict(ldaMod, newdata = training)

# Make prediction dataset
predDat <- data.frame(rfPred, gbmPred, ldaPred, diagnosis = testing$diagnosis)

# Make ensemble model using rf
ensMod <- train(diagnosis ~ ., data = predDat)

# Use ensemble model to make new prediction
ensPred <- predict(ensMod, testing)