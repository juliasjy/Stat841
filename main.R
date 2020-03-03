source("./function/getData.R")
source("./function/fillMissingDataByMeanAllNumeric.R")
source("./function/fillMissingDataByMeanCountryNonNumeric.R")

library(beepr)
train_data <- getData("train.csv")
test_data <- getData("test.csv")

#train_data <- fillMissingDataByMeanAllNumeric(train_data)
#test_data <- fillMissingDataByMeanAllNumeric(test_data)

train_data <- fillMissingDataByMeanCountryNonNumeric(train_data)
test_data <- fillMissingDataByMeanCountryNonNumeric(test_data)

train_data$satisfied <- as.factor(train_data$satisfied)

#### Split training and test data (75%, 25%) ####
set.seed(20200214)
trainSize <- floor(nrow(train_data)*0.75)
trainInx <- sample(seq_len(nrow(train_data)),size = trainSize)
train.x <- train_data[trainInx, -ncol(train_data)]
test.x <- train_data[-trainInx, -ncol(train_data)]
train.y <- train_data[trainInx, "satisfied"]
test.y <- train_data[-trainInx, "satisfied"]

## knn
library(ggplot2)
library(class)
k_range <- c(11,13,15) # range of k
err.df <- data.frame(k = rep(0, length(k_range)),
                     test.err = rep(0, length(k_range)))
i <- 1
for(k in k_range){
  knn.test.pred <- knn(train = train.x, test = test.X, cl = train.y, k = k) # knn for test data
  # knn.train.pred <- knn(train = train.x, test = train.x, cl = train.y, k = k) # knn for training data
  # train.err <- mean(train.y != knn.train.pred) # training error
  test.err <- mean(test.y != knn.test.pred) # test error
  
  # err.df each row three columns: k, training error, test error
  err.df[i,] <- c(k, test.err)
  
  i <- i+1
  beep()
}
err.df.plot <- reshape2::melt(err.df, id.vars = "k")
ggplot(err.df.plot) + geom_line(aes(x = k, y = value,
                                    color = variable)) +
  geom_point(aes(x = k, y = value,
                 color = variable)) + 
  labs(title = "Test & Training Error", x = "k", y = "Error") +
  theme(legend.position = "bottom",
        plot.margin = unit(c(.2,1,.2,.2), "cm")) + 
  scale_color_manual(name="",
                     labels=c("Training Error","Test Error"),
                     values=c("red","blue")) +
  theme_bw()

### randomForest
library(randomForest)
rf <- randomForest(x = train.x, y = train.y, xtest = test.x, ytest = test.y, ntree = 250)
beep()
testpredicted <- rf$test$predicted
err.rf <- mean(test.y != round(testpredicted)) # 0.1855
#train on whole sample set
train_data.x <- train_data[, -c(1,ncol(train_data))]
train_data.y <- train_data[,"satisfied"]
test_data.x <- test_data[,-1]
rf_final <- randomForest(x = train_data.x, y = train_data.y, xtest = test_data.x, ntree = 500)
beep()
predict.rf <- rf_final$test$predicted
write.csv(unlist(round(predict.rf)),"./resultRF.csv")

## boosting
library(gbm)
train <- cbind(train.x, train.y)
boost <- gbm(train.y~., data = train, distribution = "gaussian", n.trees = 250,
             interaction.depth = 4)
summary(boost)
testpredicted <- round(predict(boost, newdata = test.x, n.trees = 250) - 1)
err <- mean(testpredicted != test.y) # 0.1910904
#train on whole sample set
boost <- gbm(satisfied~., data = train_data, distribution = "gaussian", n.trees = 500,
             interaction.depth = 4)
predict.boosting <- round(predict(boost, newdata = test.x, n.trees = 500) - 1)
write.csv(unlist(round(predict.rf)),"./resultBoost.csv")

## adaboost
library(adabag)
library(caret)
train <- cbind(train.x, train.y)
adaboost <- boosting(train.y~., data = train, mfinal = 50)
predicttest <- predict(adaboost, test.x)
err <- mean(predicttest$class != test.y) #0.1922872
#train on whole sample set
adaboost <- boosting(satisfied~., data = train_data, mfinal = 50)
predict.ada <- predict(adaboost, test.x)
write.csv(unlist(predict.ada$class),"./resultAdaBoost.csv")

## Neural Network - doesnt work
library(neuralnet)
train <- cbind(train.x, train.y)
nn <- neuralnet(train.y~., data = train, hidden = c(3,2),
                err.fct = "ce",
                linear.output = F)
predict.nn <- predict(nn, test.x)

## Gradient Boosting
library(tidyverse)
library(caret)
library(xgboost)
train <- cbind(train.x, train.y)
gb <- caret::train(as.factor(train.y)~., data = train, method = "xgbTree",
                   trControl = trainControl("cv", number = 10))
predict.gb <- predict(gb, test.x)
sum(predict.gb != test.y)/nrow(test.x) #0.1844415
predict.gb.test <- predict(gb, test_data[,2:ncol(test_data)])
result.RF <- read.csv("./resultRF.csv", header = T)
sum(result.RF$x != predict.gb.test) # 456 different results
#train on whole sample set
gb <- caret::train(satisfied~., data = train_data, method = "xgbTree",
                   trControl = trainControl("cv", number = 10))
predict.gb <- predict(gb, test_data)
write.csv(unlist(predict.gb),"./resultGB.csv")

## SVM
library(e1071)
train <- cbind(train.x, train.y)
svm.tune <- tune(svm, train.y~., data = train, kernel = "radial",
           ranges = list(cost = c(0.1, 1, 10, 100, 1000), gamma = c(0.5, 1, 2, 3, 4)))
summary(svm.tune)
predict.svm <- predict(tune.out$best.model, test.x)
beep()

## Combine All Result
result.RF <- read.csv("./resultRF.csv", header = T)
result.GB <- read.csv("./resultGB.csv", header = T)
result.AdaBoost <- read.csv("./resultAdaBoost.csv", header = T)
result.Boost <- read.csv("./resultBoost.csv", header = T)

result <- (result.RF$x + result.GB$x + result.AdaBoost$x + result.Boost$x)/4

