library(randomForest)
require(caTools)
X_train <- read.csv("training_data.csv", header = TRUE)
X_test <- read.csv("test_data.csv", header = TRUE)

# for(mtry in 1:6){
#   forest_model=randomForest(TRX_COUNT ~ . , data = X_train, mtry = mtry,ntree=400)
#   training_scores <- predict(forest_model, X_train)
#   # mean absolute error for training data
#   means <- mean(abs(training_scores - X_train$TRX_COUNT))
#  
#   # root mean squared error for training data
#  sqrts <- sqrt(mean((training_scores - X_train$TRX_COUNT)^2))
#  
#   cat(mtry," ", means, " ", sqrts, "\n") #printing the output to the console
# }

forest_model=randomForest(TRX_COUNT ~ . , data = X_train, mtry = 6,ntree=400)
training_scores <- predict(forest_model, X_train)
# mean absolute error for training data
mean(abs(training_scores - X_train$TRX_COUNT))

# root mean squared error for training data
sqrt(mean((training_scores - X_train$TRX_COUNT)^2))
print(forest_model)
plot(forest_model)

test_scores <- predict(forest_model, X_test)
write.table(test_scores, file = "test_predictions.csv", row.names = FALSE, col.names = FALSE)