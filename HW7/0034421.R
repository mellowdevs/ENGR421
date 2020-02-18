library(AUC)
library(onehot)
library(xgboost)
library(randomForest)
library(rpart)
library(e1071)
library(caret)


for (target in 1:3) {
  X_train <- read.csv(sprintf("hw07_target%d_training_data.csv",target), header = TRUE)
  Y_train <- read.csv(sprintf("hw07_target%d_training_label.csv",target), header = TRUE)
  X_test <- read.csv(sprintf("hw07_target%d_test_data.csv",target), header = TRUE)
   partition <- createDataPartition(Y_train$ID, p = 0.9, list = FALSE)
   X_train <- X_train[partition,]
   Y_train <- Y_train[partition,]
   X_test <- X_test[partition,]

  encoder <- onehot(X_train, addNA = TRUE, max_levels = Inf)
  X_train_d <- predict(encoder, data = X_train)
  X_test_d <- predict(encoder, data = X_test)

  set.seed(421)
  boosting_model <- xgboost(data = X_train_d[, -1], label = Y_train[,"TARGET"], nrounds = 20, objective = "binary:logistic")
  training_scores <- as.matrix(predict(boosting_model, X_train_d[, -1]))
  #random forestmodelling
  randomForest_model <- randomForest(
    as.factor(Y_train$TARGET)~.,ntree =300, mtry = 8, type='classsification', na.action = na.omit, data=X_train[, !colSums(is.na(X_train)), drop = FALSE]
  )
  randomForest_model_scores = as.matrix(predict(randomForest_model, X_train))
  randomForest_model_scores = as.numeric(randomForest_model_scores)
  
  training_scores <- cbind(training_scores,randomForest_model_scores)
  
#rpart modelling
  rpart_model <- rpart(as.factor(Y_train[,"TARGET"]) ~.,
                       method="class", data=as.data.frame(X_train[,-1]))
  rpart_scores <- as.numeric(predict(rpart_model, X_train)[,1])
  training_scores <- cbind(training_scores, rpart_scores)
  
  svm_model = svm(Y_train$TARGET~ ., data = training_scores, method = "C-classification", 
                  kernel="polynomial", scale=FALSE)
  
  training_scores  <- as.matrix(predict(svm_model, training_scores))
  
  
  print(auc(roc(predictions = training_scores , labels = as.factor(Y_train[, "TARGET"]))))
  
##test
   boosting_model <- xgboost(data = X_test_d[, -1], label = Y_train[,"TARGET"], nrounds = 20, objective = "binary:logistic")
  training_scores <- as.matrix(predict(boosting_model, X_test_d[, -1]))
  # RPART modelling
  rpart_model <- rpart(as.factor(Y_train[,"TARGET"]) ~.,
                       method="class", data=as.data.frame(X_test[,-1]))
  rpart_scores <- as.numeric(predict(rpart_model, X_test)[,1])
  training_scores <- cbind(training_scores, rpart_scores)
  # RANDOMFOREST modelling
  randomForest_model <- randomForest(
    as.factor(Y_train$TARGET)~.,ntree =300, mtry = 8, type='classsification', na.action = na.omit,
    data=X_test[, !colSums(is.na(X_train)), drop = FALSE]
  )
  randomForest_model_scores = as.matrix(predict(randomForest_model, X_test))
  randomForest_model_scores = as.numeric(randomForest_model_scores)
  training_scores <-  cbind(training_scores,randomForest_model_scores)
  # SVM modelling
  svm_model = svm(Y_train$TARGET~ ., data = training_scores, method = "C-classification", 
                  kernel="polynomial", scale=FALSE)
  
  scores <- as.matrix(predict(svm_model, training_scores))
  
  # AUC score for validation data
  print(auc(roc(predictions = scores, labels = as.factor(Y_train[, "TARGET"]))))
  
  test_scores <- predict(boosting_model, X_test_d[, -1])
  write.table(cbind(ID = X_test[,"ID"], TARGET = test_scores), file = sprintf("hw07_target%d_test_predictions.csv",target), row.names = FALSE, sep = ",") 
}

