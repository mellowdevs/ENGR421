data_set <- read.csv("hw01_images.csv")
label_set <- read.csv("hw01_labels.csv")
training_set <- data_set[1:200,]
test_set <- data_set[201:399]

y_train <- cbind(label_set$X2[1:200])
y_test <- cbind(label_set$X2[201:399])

K <- max(label_set)
N <- length(label_set$X2)
female_tr_means <- sapply(X =  1:K, FUN =  function (c) {mean(training_set[y_train == 1])})
male_tr_means <- sapply(X =  1:K, FUN =  function (c) {mean(training_set[y_train == 2])})
means <- rbind(female_tr_means, male_tr_means)
print(means[,1])
print(means[,2])
means

deviation_female  <-sapply(X = 1:K, FUN = function(c) {sqrt(mean((test_set[y_train == 1] - means[,1])^2))})
deviation_male  <-sapply(X = 1:K, FUN = function(c) {sqrt(mean((test_set[y_train == 2] - means[,2])^2))})
deviations <- rbind(deviation_female, deviation_male)
print(deviations[,1])
print(deviations[,2])
deviations


priors <- rbind(sapply(X = 1:K, FUN = function(c) {mean(y_train == c)}))
priors

score_values_tr <- sapply(X = 1:N, FUN = function(c) {- 0.5 * log(2 * pi * deviations[c]^2) - 0.5 * (y_train - means[c])^2 / deviations[c]^2 + log(priors[c])})
y_hat_tr <- sapply(X=1:K, FUN = function(c) {match(max(score_values_tr[,c]),score_values_tr)})
y_train <-sapply(X = 1:N, function(c) {match(max(y_hat_tr[c]),y_hat_tr)})
score_values_test <- sapply(X = 1:N, FUN = function(c) {- 0.5 * log(2 * pi * deviations[c]^2) - 0.5 * (y_test - means[c])^2 / deviations[c]^2 + log(priors[c])})
y_hat_test <- sapply(X=1:K, FUN = function(c) {match(max(score_values_test[,c]),score_values_test)})
y_test <-sapply(X = 1:N, function(c) {match(max(y_hat_test[c]),y_hat_test)})

conf_train <- matrix(y_hat_tr, y_train)
conf_test <- matrix(y_hat_test, y_test)
