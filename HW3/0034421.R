data_set <- read.csv("hw03_images.csv",header = FALSE)
Y_truth_label <- read.csv("hw03_labels.csv",header = FALSE)
Y_truth_label <- Y_truth_label$V1

sigmoid <- function(a) {
  return (1 / (1 + exp(-a)))
}

safelog <- function(x) {
  return (log(x + 1e-100))
}



softmax <- function(Z, v) {
  s <- exp(Z %*% t(v))
  s <- s / matrix(rowSums(s), nrow(s), ncol(s), byrow = FALSE)
  return (s)
}

train_1 <- data_set[1:100,]
test_1 <- data_set[101:200,]
train_2 <- data_set[201:300,]
test_2 <- data_set[301:400,]
train_3<- data_set[401:500,]
test_3 <- data_set[501:600,]
train_4 <- data_set[601:700,]
test_4 <- data_set[701:800,]
train_5 <- data_set[801:900,]
test_5 <- data_set[901:1000,]
test_set <- rbind(test_1, test_2, test_3, test_4, test_5)
training_set <- rbind(train_1, train_2, train_3, train_4, train_5)
D <- ncol(training_set)
#training_set -> 500 obs of 784 var
#test_set -> 500 obs of 784 var

K <- 5L
N <- length(Y_truth_label)

Y_truth <- matrix(0, N,K)
Y_truth[cbind(1:N, Y_truth_label)] <- 1

Y_truth_train <- cbind(Y_truth[1:100,],Y_truth[201:300,],Y_truth[401:500,],Y_truth[601:700,],Y_truth[801:900,])
dim(Y_truth_train) <- c(500,5)
Y_truth_test <- cbind(Y_truth[101:200,],Y_truth[301:400,],Y_truth[501:600,],Y_truth[701:800,],Y_truth[901:1000,])
dim(Y_truth_test) <- c(500,5)


training_set <- data.matrix(training_set)
test_set <-  data.matrix(test_set)
#large matrix :()

eta <- 0.0005
epsilon <- 1e-3
max_iteration <- 500
H <- 20


# W  <- read.csv('initial_W.csv', header = FALSE);
# W <- data.matrix(W)
# v <- read.table('initial_V.csv', header = FALSE);
# v <- data.matrix(v)
#int[]...
D <- ncol(training_set)

set.seed(421)
 
W <- matrix(runif((D + 1) * H, min = -0.01, max = 0.01), D + 1, H)
v <- matrix(runif(100, min = -0.01, max = 0.01),5,20)
#num[1:785] W, num[1:5] V

Z <- sigmoid(cbind(1, training_set) %*% W)
y_predicted <- softmax(Z, v)
objective_values <- -sum(Y_truth_train * safelog(y_predicted))

iteration <- 1
while (1) {
  
  for (i in sample(500)) {
    Z[i,] <- sigmoid(c(1, training_set[i,]) %*% W)
    y_predicted[i,] <- softmax(Z[i,],v)  
    v <- v + eta * (Y_truth_train[i,] - y_predicted[i,]) * c(1, Z[i,])
    for (h in 1:H) {
      W[,h] <- W[,h] + eta * sum((Y_truth_train[i,] - y_predicted[i,]) * v[,h]) * Z[i, h] * (1 - Z[i, h]) * c(1, training_set[i,])
    }
  }
  
  Z <- sigmoid(cbind(1, training_set) %*% W)
  y_predicted <- softmax(Z,v) 
  objective_values <- c(objective_values, -sum(Y_truth_train * safelog(y_predicted)))
  
  if (abs(objective_values[iteration + 1] - objective_values[iteration]) < epsilon || iteration >= max_iteration) {
    break
  }
  
  iteration <- iteration + 1
}

plot(1:(iteration + 1), objective_values,
     type = "l", lwd = 2, las = 1,
     xlab = "Iter", ylab = "Error")



confusion_train <- rowSums(sapply(X=1:5, FUN=function(c) {Y_truth_train[,c]*c}))
y_predicted <- apply(y_predicted, 1, which.max)
confusion_train_matrix <- table(y_predicted, confusion_train)
print(confusion_train_matrix)

Z <- sigmoid(cbind(1, test_set) %*% W)
y_predicted <- softmax(Z,v)

confusion_test <- rowSums(sapply(X=1:5, FUN=function(c) {Y_truth_test[,c]*c}))
y_predicted <- apply(y_predicted, 1, which.max)
confusion_test_matrix <- table(y_predicted, confusion_test)
print(confusion_test_matrix)

