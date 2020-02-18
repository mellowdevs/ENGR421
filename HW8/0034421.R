library(AUC)
library(onehot)
library(caret)
library(rpart)

X_train <- read.csv("hw08_training_data.csv", header = TRUE)
Y_train <- read.csv("hw08_training_label.csv", header = TRUE)
X_test <- read.csv("hw08_test_data.csv", header = TRUE)
partition <- createDataPartition(Y_train$ID, p = 0.9, list = FALSE)
X_train <- X_train[partition,]
Y_train <- Y_train[partition,]
X_test <- X_test[partition,]



encoder <- onehot(X_train, addNA = TRUE, max_levels = Inf)
X_train_d <- predict(encoder, data = X_train)
X_test_d <- predict(encoder, data = X_test)

set.seed(421)
test_predictions <- matrix(0, nrow = nrow(X_test), ncol = ncol(Y_train))
colnames(test_predictions) <- colnames(Y_train)
test_predictions[,1] <- X_test[, 1]
outcome <- 1
for (outcome in 1:6) {
valid_customers <- which(is.na(Y_train[,outcome + 1]) == FALSE)
rpart_model <- rpart(as.factor(Y_train[valid_customers, outcome + 1]) ~.,
                     method="class", data=as.data.frame(X_train[valid_customers,-1]))
rpart_scores <- as.matrix(predict(rpart_model, X_train)[valid_customers,1])
rpart_scores <- as.numeric(rpart_scores)
sprintf("TARGET_%s TRAINING:", outcome)
# AUC score for training data
print(auc(roc(predictions = rpart_scores, labels = as.factor(Y_train[valid_customers, outcome + 1]))))



##testing
test_scores <- as.matrix(predict(rpart_model, X_test)[valid_customers, 1])
test_predictions[valid_customers, outcome + 1] <- test_scores
sprintf("TARGET_%s TESTING:", outcome)
print(auc(roc(predictions = test_scores, labels = as.factor(Y_train[valid_customers, outcome + 1]))))
}
write.table(test_predictions, file = "hw08_test_predictions.csv", row.names = FALSE, sep = ",")

