#Key points
#Most clustering and machine learning techniques rely on being able to define distance between observations, using features or predictors.
#With high dimensional data, a quick way to compute all the distances at once is to use the function dist(), which computes the distance between each row and produces an object of class dist():
  d <- dist(x)
#We can also compute distances between predictors. If  N  is the number of observations, the distance between two predictors, say 1 and 2, is:
#  dist(1,2)=∑i=1N(xi,1−xi,2)2−−−−−−−−−−−−−⎷ 
#To compute the distance between all pairs of the 784 predictors, we can transpose the matrix first and then use dist():
  d <- dist(t(x))
#Code
library(tidyverse)
library(dslabs)

if(!exists("mnist")) mnist <- read_mnist()
#set.seed(0) # if using R 3.5 or earlier
set.seed(0, sample.kind = "Rounding") # if using R 3.6 or later
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)

#the predictors are in x and the labels in y
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]

y[1:3]

x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]
x_1
#distance between two numbers
sqrt(sum((x_1 - x_2)^2))
sqrt(sum((x_1 - x_3)^2))
sqrt(sum((x_2 - x_3)^2))

#compute distance using matrix algebra
sqrt(crossprod(x_1 - x_2))
sqrt(crossprod(x_1 - x_3))
sqrt(crossprod(x_2 - x_3))

#compute distance between each row
d <- dist(x)
class(d)
as.matrix(d)[1:3,1:3]

#visualize these distances
image(as.matrix(d))

#order the distance by labels
image(as.matrix(d)[order(y), order(y)])

#compute distance between predictors
d <- dist(t(x))
dim(as.matrix(d))

d_492 <- as.matrix(d)[492,]

image(1:28, 1:28, matrix(d_492, 28, 28))

#Assessment

library(dslabs)
data(tissue_gene_expression)

dim(tissue_gene_expression$x)

table(tissue_gene_expression$y)

#Q1
d <- dist(tissue_gene_expression$x)
dim(as.matrix(d))

#Q2

as.matrix(d)[c(1,2,39,40,73,74), c(1,2,39,40,73,74)]

#Q3
image(as.matrix(d))

####KNN

#Key points
#K-nearest neighbors (kNN) estimates the conditional probabilities in a similar way to bin smoothing. However, kNN is easier to adapt to multiple dimensions.
#Using kNN, for any point  (x1,x2)  for which we want an estimate of  p(x1,x2) , we look for the k nearest points to  (x1,x2)  and take an average of the 0s and 1s associated with these points. We refer to the set of points used to compute the average as the neighborhood. Larger values of k result in smoother estimates, while smaller values of k result in more flexible and more wiggly estimates. 
#To implement the algorithm, we can use the knn3() function from the caret package. There are two ways to call this function:
#  We need to specify a formula and a data frame. The formula looks like this:  outcome∼predictor1+predictor2+predictor3 . The predict() function for knn3 produces a probability for each class.
#We can also call the function with the first argument being the matrix predictors and the second a vector of outcomes, like this:
x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x,y)

Code
library(tidyverse)
library(dslabs)
data("mnist_27")
mnist_27$test %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

#logistic regression
library(caret)
fit_glm <- glm(y~x_1+x_2, data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]

#fit knn model
knn_fit <- knn3(y ~ ., data = mnist_27$train)

#Matrix approach
x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x, y)

knn_fit <- knn3(y ~ ., data = mnist_27$train, k=5)

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

####
#Key points
#Over-training is the reason that we have higher accuracy in the train set compared to the test set. Over-training is at its worst when we set  k=1 . With  k=1 , the estimate for each  (x1,x2)  in the training set is obtained with just the  y  corresponding to that point. 
#When we try a larger  k , the  k  might be so large that it does not permit enough flexibility. We call this over-smoothing.
#Note that if we use the test set to pick this  k , we should not expect the accompanying accuracy estimate to extrapolate to the real world. This is because even here we broke a golden rule of machine learning: we selected the  k  using the test set. Cross validation also provides an estimate that takes this into account.
#Code
y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class") 
confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"]
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")  
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

#fit knn with k=1
knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$train$y)$overall[["Accuracy"]]

y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$test$y)$overall[["Accuracy"]]

#fit knn with k=401
knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_401, reference=mnist_27$test$y)$overall["Accuracy"]

#pick the k in knn
ks <- seq(3, 251, 2)
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(train = train_error, test = test_error)
})

  
  #pick the k that maximizes accuracy using the estimates built on the test data
  ks[which.max(accuracy$test)]
  max(accuracy$test)
  
##Assessment KNN

library(dslabs)
library(tidyverse)
library(caret)
data("heights")

#Q1
  
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]     
  
ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% 
    factor(levels = levels(train_set$sex))
    F_meas(data = y_hat, reference = test_set$sex)
  })
plot(ks, F_1)
max(F_1)
ks[which.max(F_1)]

#Q2

library(dslabs)
library(tidyverse)
library(caret)
data("tissue_gene_expression")

y <- tissue_gene_expression$y
x <- tissue_gene_expression$x

set.seed(1, sample.kind = "Rounding")

test_index <- createDataPartition(tissue_gene_expression$y, times = 1, p = 0.5, list = FALSE)
test_set_x <- x[test_index,]
train_set_x <- x[-test_index,]
test_set_y <- y[test_index]
train_set_y <- y[-test_index]

ks <- seq(1, 11, 2)

accuracy <- sapply(ks, function(k){
  fit <- knn3(train_set_x, train_set_y, k = k)
  y_hat <- predict(fit, test_set_x, type = "class")
  match <- confusionMatrix(data = y_hat, reference = test_set_y)$overall["Accuracy"]
  list(k=k, match=match)
})
accuracy

####Cross-validation
#Key points
#For  k -fold cross validation, we divide the dataset into a training set and a test set. We train our algorithm exclusively on the training set and use the test set only for evaluation purposes. 
#For each set of algorithm parameters being considered, we want an estimate of the MSE and then we will choose the parameters with the smallest MSE. In  k -fold cross validation, we randomly split the observations into  k  non-overlapping sets, and repeat the calculation for MSE for each of these sets. Then, we compute the average MSE and obtain an estimate of our loss. Finally, we can select the optimal parameter that minimized the MSE.
#In terms of how to select  k  for cross validation, larger values of  k  are preferable but they will also take much more computational time. For this reason, the choices of  k=5  and  k=10  are common.

##Assessment
library(tidyverse)
library(caret)

# set.seed(1996) #if you are using R 3.5 or earlier
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

#Q1
?train
fit <- train(x_subset, y, method = "glm")
fit$results

#Q2
install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)

pvals <- tt$p.value

#Q3
ind <- tt$p.value<=0.01
mean(ind) * 10000

#or
ind <- which(pvals <= 0.01)
length(ind)

#Q4
x_subset <- x[ ,ind]
fit <- train(x_subset, y, method = "glm")
fit$results

#Q5
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

#Q6
#Because we used the entire dataset to select the columns in the model, the accuracy is too high. The selection step needs to be included as part of the cross-validation algorithm, and then the cross-validation itself is performed after the column selection step.
#As a follow-up exercise, try to re-do the cross-validation, this time including the selection step in the cross-validation algorithm. The accuracy should now be close to 50%.

#Q7
library(dslabs)
data("tissue_gene_expression")
fit <- train(tissue_gene_expression$x, tissue_gene_expression$y, method = "knn", tuneGrid = data.frame(k = seq(1, 7, 2)))
ggplot(fit)
fit$results

####
#Key points
#When we don't have access to the entire population, we can use bootstrap to estimate the population median  m  .
#The bootstrap permits us to approximate a Monte Carlo simulation without access to the entire distribution. The general idea is relatively simple. We act as if the observed sample is the population. We then sample datasets (with replacement) of the same sample size as the original dataset. Then we compute the summary statistic, in this case the median, on this bootstrap sample.
#Note that we can use ideas similar to those used in the bootstrap in cross validation: instead of dividing the data into equal partitions, we simply bootstrap many times.
#Code
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))

m <- median(income)
m

set.seed(1, sample.kind="Rounding")
N <- 250
X <- sample(income, N)
M<- median(X)
M

library(gridExtra)
B <- 10^5
Ms <- replicate(B, {
    X <- sample(income, N)
    median(X)
})
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M)) + geom_abline()
grid.arrange(p1, p2, ncol = 2)

mean(M)
sd(M)

B <- 10^5
M_star <- replicate(B, {
    X_star <- sample(X, N, replace = TRUE)
    median(X_star)
})

library(tidyverse)
tibble(monte_carlo = sort(M), bootstrap = sort(M_star)) %>%
    qplot(monte_carlo, bootstrap, data = .) + 
    geom_abline()

quantile(M, c(0.05, 0.95))
quantile(M_star, c(0.05, 0.95))

median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1)

mean(M) + 1.96 * sd(M) * c(-1,1)

mean(M_star) + 1.96 * sd(M_star) * c(-1, 1)

#Assessment
library(dslabs)
library(caret)
data(mnist_27)
# set.seed(1995) # if R 3.5 or earlier
set.seed(1995, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(mnist_27$train$y, 10)

#Q1
data.frame(sum(indexes[[1]]==1), sum(indexes[[1]]==4), sum(indexes[[1]]==7))

#Q2
df <- as.data.frame(indexes)
sum(df==3)

#or
x=sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(x)

#Q3
qnorm(0.75)
quantile(y, 0.75)

set.seed(1, sample.kind="Rounding")
B <- 10000
MC <- replicate(B, {
  y <- rnorm(100, 0, 1)
  qf <- quantile(y, 0.75)
})

mean(MC)
sd(MC)

#Q4
library(tidyverse)
library(caret)
set.seed(1, sample.kind = "Rounding")
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind = "Rounding")
indexes <- createResample(y, 10)
i <- 1:10
res <- sapply(i, function(i) {
  ind <- y[indexes[[i]]]
  quantile(ind, 0.75)
})
mean(res)
sd(res)

#or

set.seed(1, sample.kind = "Rounding")
y <- rnorm(100, 0, 1)

set.seed(1, sample.kind="Rounding")
indexes <- createResample(y, 10)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)

#Q5
library(tidyverse)
library(caret)
set.seed(1, sample.kind = "Rounding")
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind = "Rounding")
indexes <- createResample(y, 10000)

res <- sapply(indexes, function(ind) {
  x <- y[ind]
  quantile(x, 0.75)
})
mean(res)
sd(res)