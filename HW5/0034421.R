data_set <- read.csv("hw05_data_set.csv")
x <- data_set$eruptions
y <- data_set$waiting

train_indices <- sample(1:150)
x_train <- x[1:150]
y_train <- y[1:150]
x_test <- x[151:272]
y_test <- y[151:272]
minimum_value <- floor(min(x)) - 2 
maximum_value <- ceiling(max(x)) + 2
N_train <- length(x_train)
N_test <- length(x_test)
K <- max(y)


DT_Function <- function(P) {
  node_splits <- c()
  node_means <- c()
  node_indices <- list(1:150)
  is_terminal <- c(FALSE)
  need_split <- c(TRUE)
  
  while (1) {
    split_nodes <- which(need_split)
    if (length(split_nodes) == 0) {
      break
    }
    for (split_node in split_nodes) {
      data_indices <- node_indices[[split_node]]
      need_split[split_node] <- FALSE
      node_mean <- mean(y_train[data_indices])
      if (length(x_train[data_indices]) <= P) {
        is_terminal[split_node] <- TRUE
        node_means[split_node] <- node_mean
      } else {
        is_terminal[split_node] <- FALSE
        unique_values <- sort(unique(x_train[data_indices]))
        split_positions <- (unique_values[-1] + unique_values[-length(unique_values)]) / 2
        split_scores <- rep(0, length(split_positions))
        for (s in 1:length(split_positions)) {
          left_indices <- data_indices[which(x_train[data_indices] <= split_positions[s])]
          right_indices <- data_indices[which(x_train[data_indices] > split_positions[s])]
          total_error <- 0
          if (length(left_indices) > 0) {
            mean <- mean(y_train[left_indices])
            total_error <- total_error + sum((y_train[left_indices] - mean) ^ 2)
          }
          if (length(right_indices) > 0) {
            mean <- mean(y_train[right_indices])
            total_error <- total_error + sum((y_train[right_indices] - mean) ^ 2)
          }
          split_scores[s] <- total_error / (length(left_indices) + length(right_indices))
        }
        if (length(unique_values) == 1) {
          is_terminal[split_node] <- TRUE
          node_means[split_node] <- node_mean
          next 
        }
        best_split <- split_positions[which.min(split_scores)]
        node_splits[split_node] <- best_split
  
        left_indices <- data_indices[which(x_train[data_indices] < best_split)]
        node_indices[[2 * split_node]] <- left_indices
        is_terminal[2 * split_node] <- FALSE
        need_split[2 * split_node] <- TRUE
        
        right_indices <- data_indices[which(x_train[data_indices] >= best_split)]
        node_indices[[2 * split_node + 1]] <- right_indices
        is_terminal[2 * split_node + 1] <- FALSE
        need_split[2 * split_node + 1] <- TRUE
      }
    }
  }
  return(list("splits"= node_splits, "means"= node_means, "is_terminal"= is_terminal))
}
P <- 25
DT <- DT_Function(P)
node_splits <- DT$splits
node_means <- DT$means
is_terminal <- DT$is_terminal

regression <- function(reg, is_terminal, node_splits, node_means){
  i <- 1
  while (1) {
    if (is_terminal[i] == TRUE) {
      return(node_means[i])
    } else {
      if (reg <= node_splits[i]) {
        i <- i * 2
      } else {
        i <- i * 2 + 1
      }
    }
  }
}
grid_interval <- 0.01
data_interval <- seq(from = minimum_value, to = maximum_value, by = grid_interval)

plot(x_train, y_train, type = "p", pch = 19, col = "lightblue",
     ylim = c(min(y_train), max(y_train)), xlim = c(minimum_value, maximum_value),
     ylab = "Waiting time to next eruption (min)", xlab = "Eruption time (min)", las = 1)
points(x_test, y_test, type = "p", pch = 19, col= "red")
legend("topleft", legend=c("training", "test"),
       col=c("lightblue", "red"), pch = 19, cex = 1)

for (b in 1:length(data_interval)) {
  x_left <- data_interval[b]
  x_right <- data_interval[b+1]
  lines(c(x_left, x_right), c(regression(x_left, is_terminal, node_splits, node_means), regression(x_left, is_terminal, node_splits, node_means)), lwd = 2, col = "black")
  if (b < length(data_interval)) {
    lines(c(x_right, x_right), c(regression(x_left, is_terminal, node_splits, node_means), regression(x_right, is_terminal, node_splits, node_means)), lwd = 2, col = "black") 
  }
}

##Calculate RMSE##
y_hat <- sapply(X=1:N_test, FUN = function(i) regression(x_test[i], is_terminal, node_splits, node_means))
RMSE <- sqrt(sum((y_test - y_hat) ^ 2) / length(y_test))
sprintf("RMSE is %g when P is %s", RMSE, P)

seq <- seq(5, 50, length.out = 10)

RMSE_learner <- sapply(X=seq, FUN = function(p) {
  DT <- DT_Function(p)
  node_splits <- DT$splits 
  node_means <- DT$means 
  is_terminal <- DT$is_terminal
  y_test_predicted <- sapply(X=1:N_test, FUN = function(i) regression(x_test[i], is_terminal, node_splits, node_means))
  RMSE <- sqrt(sum((y_test - y_test_predicted) ^ 2) / length(y_test))
})


plot(seq, RMSE_learner,
     type = "o", lwd = 1, las = 1, pch = 16, lty = "solid", 
     xlim = range(50, 5),xlab = "Pre-pruning size (P)", ylab = "RMSE")
 
