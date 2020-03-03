source("./function/getData.R")
source("./function/fillMissingDataByMean.R")

library(beepr)
train_data <- getData("train.csv")
train_data <- fillMissingDataByMean(train_data)

test_data <- getData("test.csv")
test_data <- fillMissingDataByMean(test_data)

library(class)
library(ggplot2)

#### Split training and test data (75%, 25%) ####
set.seed(20200214)
trainSize <- floor(nrow(train_data)*0.75)
trainInx <- sample(seq_len(nrow(train_data)),size = trainSize)
train.x <- train_data[trainInx, 2:(ncol(train_data)-1)]
test.x <- train_data[-trainInx, 2:(ncol(train_data)-1)]
train.y <- as.factor(train_data[trainInx, ncol(train_data)])
test.y <- as.factor(train_data[-trainInx, ncol(train_data)])

## knn
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
rf <- randomForest(x = train.x, y = train.y, xtest = test.X, ytest = test.y, ntree = 250)
beep()
testpredicted <- rf$test$predicted
err <- sum(test.y != round(testpredicted))/length(test.y)

train.x <- train_data[, -c(1,ncol(train_data))]
train.y <- train_data[,"satisfied"]
test.x <- test_data[,-1]
rf_final <- randomForest(x = train.x, y = train.y, xtest = test.x, ntree = 250)
beep()
testpredicted <- rf_final$test$predicted
write.csv(unlist(round(testpredicted)),"./result.csv")

## boosting
library(gbm)
train <- cbind(train.x, train.y)
boost <- gbm(train.y~., data = train, distribution = "gaussian", n.trees = 250,
             interaction.depth = 4)
summary(boost)
predict.y <- round(predict(boost, newdata = test.x, n.trees = 250))
mean((predict.y - test.y)^2)

## adaboost - doesnt work
library(adabag)
library(caret)
train <- cbind(train.x, train.y)
adaboost <- boosting(train.y~., data = train, mfinal = 50)

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
result <- read.csv("./result.csv", header = T)
sum(result$x != predict.gb.test) # 456 different results
write.csv(unlist(predict.gb.test),"./resultGB.csv")

## SVM
library(e1071)
svm <- svm(train.y~., data = train, kernel = "linear",
           cost = 10, scale = F)
predict.svm <- predict(svm, test.x)