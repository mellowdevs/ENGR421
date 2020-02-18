library(AUC)
library(onehot)
library(tidyverse)
library(caret)
library(xgboost)
library(randomForest)
library(e1071)
library(ada)

# for (target in 1:3) {
  # X_train <- read.csv(sprintf("hw07_target%d_training_data.csv", target), header = TRUE)
  # Y_train <- read.csv(sprintf("hw07_target%d_training_label.csv", target), header = TRUE)
  # X_test <- read.csv(sprintf("hw07_target%d_test_data.csv", target), header = TRUE)
#   
  # encoder <- onehot(X_train, addNA = TRUE, max_levels = Inf)
  # X_train_d <- predict(encoder, data = X_train)
  # X_test_d <- predict(encoder, data = X_test)

#   set.seed(421)
#   boosting_model <- xgboost(data = X_train_d[, -1], label = Y_train[,"TARGET"],nrounds = 50, objective = "binary:logistic")
  # training_scores <- predict(boosting_model, X_test_d[, -1])
  # # AUC score for training data
  # print(auc(roc(predictions = training_scores, labels = as.factor(Y_train[, "TARGET"]))))
#   test_scores <- predict(boosting_model, X_test_d[, -1])
#   write.table(cbind(ID = X_test[,"ID"], TARGET = test_scores), file = sprintf("hw07_target%d_test_predictions.csv", target), row.names = FALSE, sep = ",") 
# }

X_train <- read.csv(sprintf("hw07_target1_training_data.csv"), header = TRUE)
Y_train <- read.csv(sprintf("hw07_target1_training_label.csv"), header = TRUE)
X_test <- read.csv(sprintf("hw07_target1_test_data.csv"), header = TRUE)

training_set <- merge(X_train, Y_train)

encoder <- onehot(X_train, addNA = TRUE, max_levels = Inf)

X_train_d <- predict(encoder, data = training_set)
X_test_d <- predict(encoder, data = X_test)



boosting_model <- xgboost(data = X_train_d[, -1], label = Y_train[,"TARGET"],nrounds = 50, objective = "binary:logistic")
training_scores <- predict(boosting_model, X_test_d[, -1])
# AUC score for training data
print(auc(roc(predictions = training_scores, labels = as.factor(Y_train[, "TARGET"]))))

test_scores <- predict(boosting_model, X_test_d[, -1])
mellow <- cbind(ID = X_test[,"ID"], TARGET = test_scores)
