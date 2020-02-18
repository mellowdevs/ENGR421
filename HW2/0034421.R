library(MASS)
safelog <- function(x) {
  return (log(x + 1e-100))
}


epsilon <- 1e-3
max_iteration <- 500

error <- function(y, y_pred){
  return (sum(sum((y_pred - y)^2))) * 0.5
}

sigmoid <- function(a) {
  return (1 / (1 + exp(-a)))
}


data <- read.csv("hw02_images.csv", header = FALSE)
labels <- read.csv("hw02_labels.csv")
labels <- labels$X1
w <- read.table('initial_W.csv', header= FALSE);
w <- cbind(w$V1)
w0 <- read.table('initial_w0.csv')


training_set <- data.matrix(data[1:500,])
test_set <- data[501: 1000,]

y_train <-  labels[1:500]
dim(y_train) <- c(100, 5)
y_test <- labels[501: 1000]

K <- max(y_train)
N <- length(y_train)
y_pred_train <- matrix(0, N, 5)
y_pred_test <- matrix(0,N,5)
y_pred_train[cbind(1:N, y_pred_test)] <- 1
dim(y_pred_train) <- c(N,5)
error_history <- matrix(0,max_iteration, 1)


gradient_W <- function(X, Y_truth, Y_predicted) {
  return (-sapply(X = 1:ncol(Y_truth), function(c) colSums(matrix(Y_truth[,c] - Y_predicted[,c], nrow = nrow(X), ncol = ncol(X), byrow = FALSE) * X)))
}
gradient_w0 <- function(Y_truth, Y_predicted) {
  return (-colSums(Y_truth - Y_predicted))
}
objective_values <-c()
iteration <- 1
err <- 0
eta<- 0.0001
while (iteration < 3) {
  
  Y_predicted <- sapply(X = 1:N, FUN = function(c) {sigmoid(training_set[c,] %*% w + w0)})
  Y_predicted <- data.frame(Y_predicted)
  Y_predicted <- t(Y_predicted)
  err <- error(y_pred_train + epsilon, Y_predicted)
  objective_values[iteration] <- err
  W_old <- w
  w0_old <- w0
  w <- c(w) + eta * gradient_W(training_set, y_pred_train, Y_predicted)
  w0 <- w0 + eta * gradient_w0(y_pred_train, Y_predicted)
  
  if (sqrt(sum((w0 - w0_old)^2) + sum((w - W_old)^2)) < epsilon) {
    break
  }
  
  iteration <- iteration + 1
  print(iteration)
}
print(w)
print(w0)
print(iteration)
plot(1:iteration, objective_values[1:3], type = "l", lwd = 2, las = 1, xlab = "Iteration", ylab = "Error")

y_predicted <- apply(Y_predicted, 1, which.max)
confusion_matrix <- table(y_predicted, y_train)
print(confusion_matrix)


y_predicted <- apply(Y_predicted, 1, which.max)
confusion_matrix <- table(y_predicted, y_test)
print(confusion_matrix)

