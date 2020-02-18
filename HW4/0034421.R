data_set <- read.csv("hw04_data_set.csv")
training_set <- data_set[1:150,]
test_set <- data_set[151:272,]


x_train <- training_set$eruptions
y_train <- training_set$waiting
x_test <- test_set$eruptions
y_test <- test_set$waiting

K <- max(y_train)
N <- length(y_train)

point_colors <- c("red", "lightblue")
minimum_value <- 1.5
maximum_value <- 5
grid<- 0.01
data_interval <- seq(from = minimum_value, to = maximum_value, by = grid)

bin_width <- 0.37
left_borders <- seq(from = minimum_value, to = maximum_value - bin_width, by = bin_width)
right_borders <- seq(from = minimum_value + bin_width, to = maximum_value, by = bin_width)


################
##REGRESSOGRAM##
################



p_head_r <- sapply(1:length(left_borders), function(i) {sum(y_train[left_borders[i] < x_train & x_train <= right_borders[i]])/sum(left_borders[i] < x_train & x_train <= right_borders[i])})
#display

plot(x_train, y_train, type = "p", pch = 20,
     ylim = c(min(y_train), max(y_train)), xlim = c(minimum_value, maximum_value),
     ylab = "density", xlab = "x", las = 1, main = sprintf("h = %g", bin_width),col="lightblue")
points(x_test, y_test,
       col = "red", pch = 20)
legend("topleft", legend=c("Training", "Test"), col=c("lightblue", "red"), pch= 20, cex=0.6)
#draw line
for (i in 1:length(left_borders)) {
  lines(c(left_borders[i], right_borders[i]), c(p_head_r[i], p_head_r[i]), lwd = 2, col = "black")
  if (i < length(left_borders)) {
    lines(c(right_borders[i], right_borders[i]), c(p_head_r[i], p_head_r[i + 1]), lwd = 2, col = "black")
  }
}


#RMSE
RMSE <- 0
for (i in 1:length(left_borders)) {
  RMSE <- RMSE + sum((y_test[left_borders[i] < x_test & x_test <= right_borders[i]]-p_head_r[i])^2)
}
RMSE <- sqrt(RMSE/122)

sprintf("Regressogram =>RMSE is %g when h is %s", RMSE, bin_width)




###########################
###Running Mean Smoother###
###########################

p_head_rm <- sapply(data_interval, function(x) {
  y_train_ <- y_train[(x - bin_width *  1/2) < x_train & x_train <= (x + bin_width *  1/2)]
  return(mean(y_train_))
})
#display
plot(x_train, y_train, type = "p", pch = 20, 
     ylim = c(min(y_train), max(y_train)), xlim = c(minimum_value, maximum_value),
     ylab = "Waiting time to next eruption (min)", xlab = "Eruption time (min)", las = 1, main = sprintf("h = %g", bin_width),col="lightblue")
points(x_test, y_test,
       col = "red", pch = 20)
legend("topleft", legend=c("Training", "Test"), col=c("lightblue", "red"), pch= 20, cex=0.6)
#draw line
lines(data_interval, p_head_rm, type = "l", lwd = 2, col = "black")

#RMSE
RMSE <- 0
for (i in 1:length(left_borders)) {
  RMSE <- RMSE + sum((y_test[left_borders[i] < x_test & x_test <= right_borders[i]]-p_head_rm[i])^2)
}
RMSE <- sqrt(RMSE/122)
sprintf("Running Mean Smoother => RMSE is %g when h is %s", RMSE, bin_width)

#####################
###Kernel Smoother###
#####################

p_head_k <- sapply(data_interval, function(x) {sum(1 / sqrt(2 * pi) * exp(-0.5 * (x - x_train)^2 / bin_width^2) * y_train) /sum(1 / sqrt(2 * pi) * exp(-0.5 * (x - x_train)^2 / bin_width^2))})

plot(x_train, y_train, type = "p", pch = 20, 
     ylim = c(min(y_train), max(y_train)), xlim = c(minimum_value, maximum_value),
     ylab = "Waiting time to next eruption (min)", xlab = "Eruption time (min)", las = 1, main = sprintf("h = %g", bin_width),col="lightblue")
points(x_test, y_test,
       col = "red", pch = 20)
legend("topleft", legend=c("Training", "Test"), col=c("lightblue", "red"), pch= 20, cex=0.6)
#draw line
lines(data_interval, p_head_k, type = "l", lwd = 2, col = "black")

#RMSE
RMSE <- 0
for (i in 1: length(left_borders)) {
  RMSE <- RMSE + sum((y_test[left_borders[i] < x_test & x_test <= right_borders[i]]-p_head_k[i])^2)
}

RMSE <- sqrt(RMSE/122)
sprintf("Kernel Smoother => RMSE is %g when h is %s", RMSE, bin_width)


