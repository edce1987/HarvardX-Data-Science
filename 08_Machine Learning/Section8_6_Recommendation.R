#Key points
#We will apply what we have learned in the course on the Modified National Institute of Standards and Technology database (MNIST) digits, a popular dataset used in machine learning competitions. 
#Code
library(dslabs)
mnist <- read_mnist()

names(mnist)
dim(mnist$train$images)

class(mnist$train$labels)
table(mnist$train$labels)

# sample 10k rows from training set, 1k rows from test set
set.seed(123)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 1000)
#note that the line above is the corrected code - code in video at 0:52 is incorrect
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])

##Pre-processing

#Key points
#Common preprocessing steps include:
#  standardizing or transforming predictors and
#removing predictors that are not useful, are highly correlated with others, have very few non-unique values, or have close to zero variation. 
#Code
library(matrixStats)
library(tidyverse)
x
sds <- colSds(x)
qplot(sds, bins = 256, color = I("black"))

library(caret)
nzv <- nearZeroVar(x)
image(matrix(1:784 %in% nzv, 28, 28))

col_index <- setdiff(1:ncol(x), nzv)
length(col_index)

##
#Key points
#The caret package requires that we add column names to the feature matrices.
#In general, it is a good idea to test out a small subset of the data first to get an idea of how long your code will take to run.
#Code
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(x)

control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(x[,col_index], y,
                   method = "knn", 
                   tuneGrid = data.frame(k = c(1,3,5,7)),
                   trControl = control)
ggplot(train_knn)

n <- 1000
b <- 2
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index ,col_index], y[index],
                   method = "knn",
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)
fit_knn <- knn3(x[ ,col_index], y,  k = 3)

y_hat_knn <- predict(fit_knn,
                     x_test[, col_index],
                     type="class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]

cm$byClass[,1:2]

library(Rborist)
control <- trainControl(method="cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1,5) , predFixed = c(10, 15, 25, 35, 50))
train_rf <-  train(x[, col_index], y,
                   method = "Rborist",
                   nTree = 50,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)
ggplot(train_rf)
train_rf$bestTune

fit_rf <- Rborist(x[, col_index], y,
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed)

y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"]

rafalib::mypar(3,4)
for(i in 1:12){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste("Our prediction:", y_hat_rf[i]),
        xaxt="n", yaxt="n")
}

####
#Key points
#The Rborist package does not currently support variable importance calculations, but the randomForest package does.
#An important part of data science is visualizing results to determine why we are failing.
#Code
library(randomForest)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])
rf <- randomForest(x, y,  ntree = 50)
imp <- importance(rf)
imp

library(tidyverse)
plot(rf)

image(matrix(imp, 28, 28))

p_max <- predict(fit_knn, x_test[,col_index])
p_max <- apply(p_max, 1, max)
ind  <- which(y_hat_knn != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]
rafalib::mypar(3,4)
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1],
        main = paste0("Pr(",y_hat_knn[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}

p_max <- predict(fit_rf, x_test[,col_index])$census  
p_max <- p_max / rowSums(p_max)
p_max <- apply(p_max, 1, max)
ind  <- which(y_hat_rf != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]
rafalib::mypar(3,4)
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste0("Pr(",y_hat_rf[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}

####
#Key points
#Ensembles combine multiple machine learning algorithms into one model to improve predictions.
#Code
library(caret)
library(Rborist)
p_rf <- predict(fit_rf, x_test[,col_index])$census
p_rf <- p_rf / rowSums(p_rf)
p_knn <- predict(fit_knn, x_test[,col_index])
p <- (p_rf + p_knn)/2
y_pred <- factor(apply(p, 1, which.max)-1)
confusionMatrix(y_pred, y_test)

#Assessment Ensemble
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

library(caret)
library(dslabs)
library(tidyverse)
set.seed(1, sample.kind = "Rounding")
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models
fits

#Q1
length(mnist_27$test$y)
length(models)
fits
preds <- sapply(fits, function(fit){
  predict(fit, mnist_27$test)
})
dim(preds)
View(preds)
class(preds)

#Q3
predictions <- data.frame(preds)
accuracy <- sapply(predictions, function(p){
  cf <- confusionMatrix(data = factor(p), factor(mnist_27$test$y))
  res <- cf$overall["Accuracy"]
})

View(accuracy)

mean(accuracy)

#or
acc <- colMeans(pred == mnist_27$test$y)
acc
mean(acc)

#Q4
votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

#Q5
accuracy[accuracy > 0.81]

#Q6
mean(fits[[1]]$results$Accuracy)

acc_hat <- sapply(fits, function(fit) {
  min(fit$results$Accuracy)
})
mean(acc_hat)

#Q7
ind <- acc_hat >= 0.8
votes <- rowMeans(pred[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)

####Recommendation Systems
#Netflix Challenge links
#For more information about the "Netflix Challenge," you can check out these sites:
#  
#  https://bits.blogs.nytimes.com/2009/09/21/netflix-awards-1-million-prize-and-starts-a-new-contest/ External link
#http://blog.echen.me/2011/10/24/winning-the-netflix-prize-a-summary/ External link
#https://www.netflixprize.com/assets/GrandPrize2009_BPC_BellKor.pdf External link
#Key points
#Recommendation systems are more complicated machine learning challenges because each outcome has a different set of predictors. For example, different users rate a different number of movies and rate different movies.
#To compare different models or to see how well we’re doing compared to a baseline, we will use root mean squared error (RMSE) as our loss function. We can interpret RMSE similar to standard deviation.
#If  N  is the number of user-movie combinations,  yu,i  is the rating for movie  i  by user  u , and  y^u,i  is our prediction, then RMSE is defined as follows: 
#  1N∑u,i(y^u,i−yu,i)2−−−−−−−−−−−−−−−√ 
#Code
#Please refer to the textbook for an updated version of the code that may contain some corrections.

library(dslabs)
library(tidyverse)
data("movielens")

head(movielens)

movielens %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

keep <- movielens %>%
  dplyr::count(movieId) %>%
  top_n(5) %>%
  pull(movieId)
tab <- movielens %>%
  filter(userId %in% c(13:20)) %>% 
  filter(movieId %in% keep) %>% 
  select(userId, title, rating) %>% 
  spread(title, rating)
tab %>% knitr::kable()

users <- sample(unique(movielens$userId), 100)
rafalib::mypar()
movielens %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")

movielens %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

movielens %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  ggtitle("Users")

library(caret)
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

####
#Key points
#We start with a model that assumes the same rating for all movies and all users, with all the differences explained by random variation: If  μ  represents the true rating for all movies and users and  ϵ  represents independent errors sampled from the same distribution centered at zero, then: 
#  Yu,i=μ+ϵu,i 
#In this case, the least squares estimate of  μ  — the estimate that minimizes the root mean squared error — is the average rating of all movies across all users.
#We can improve our model by adding a term,  bi , that represents the average rating for movie  i :
#  Yu,i=μ+bi+ϵu,i 
#bi  is the average of  Yu,i  minus the overall mean for each movie  i .#
#
#We can further improve our model by adding  bu , the user-specific effect:
#  Yu,i=μ+bi+bu+ϵu,i 
#Note that because there are thousands of  b 's, the lm() function will be very slow or cause R to crash, so we don’t recommend using linear regression to calculate these effects.
#Code
#Please refer to the textbook for an updated version of the code that may contain some corrections.
mu_hat <- mean(train_set$rating)
mu_hat

naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

predictions <- rep(2.5, nrow(test_set))
RMSE(test_set$rating, predictions)

rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

#fit <- lm(rating ~ as.factor(userId), data = movielens)
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
     group_by(movieId) %>% 
     summarize(b_i = mean(rating - mu))

movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- mu + test_set %>% 
     left_join(movie_avgs, by='movieId') %>%
     .$b_i

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))

rmse_results %>% knitr::kable()

train_set %>% 
     group_by(userId) %>% 
     summarize(b_u = mean(rating)) %>% 
     filter(n()>=100) %>%
     ggplot(aes(b_u)) + 
     geom_histogram(bins = 30, color = "black")

# lm(rating ~ as.factor(movieId) + as.factor(userId))
user_avgs <- test_set %>% 
     left_join(movie_avgs, by='movieId') %>%
     group_by(userId) %>%
     summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
     left_join(movie_avgs, by='movieId') %>%
     left_join(user_avgs, by='userId') %>%
     mutate(pred = mu + b_i + b_u) %>%
     .$pred

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

####Assessment
library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")

movielens %>% select(movieId, title, year, rating) %>% 
  group_by(year) %>% 
  summarise(n=n(), m=median(n)) %>% arrange(desc(m))

#or
movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Q2
dat <- movielens %>% filter(year >= 1993) %>%
  group_by(title) %>% 
  summarise(n=n(), rpy=mean(n/(2018-year)), avg_rating=mean(rating)) %>% 
  arrange(desc(n))# %>% 
  top_n(25, n)
  
#or
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate))

#Q3
dat %>% ggplot(aes(avg_rating, rpy)) + geom_point() + geom_smooth()

#Q4
mean(movielens$rating)
#Lower than average

#Q5
movielens <- mutate(movielens, date = as_datetime(timestamp))

#Q6
movielens %>% 
  mutate(rd=round_date(date, unit="week")) %>%
  group_by(rd) %>% summarise(n=n(), avg_rating=mean(rating)) %>%
  ggplot(aes(rd, avg_rating)) + geom_line() + geom_smooth()

movielens %>% 
  mutate(rd=round_date(date, unit="week")) %>%
  group_by(rd) %>% summarise(n=n(), avg_rating=mean(rating)) %>% 
  lm(avg_rating ~ rd, data = .) %>% summary()

#Q7
#Yu,i=μ+bi+bu+f(du,i)+εu,i , with  f  a smooth function of  du,i

#Q8
movielens %>% group_by(genres) %>% 
  summarise(n=n(), avg_rating=mean(rating), sd_rating=sd(rating)) %>%
  filter(n>1000) %>% arrange(avg_rating)

####
#Notes
#To improve our results, we will use regularization. Regularization constrains the total variability of the effect sizes by penalizing large estimates that come from small sample sizes.
#To estimate the  b ’s, we will now minimize this equation, which contains a penalty term:
#  1N∑u,i(yu,i−μ−bi)2+λ∑ib2i 
#The first term is the mean squared error and the second is a penalty term that gets larger when many  b ’s are large.#
#
#The values of  b  that minimize this equation are given by:
#  b^i(λ)=1λ+ni∑u=1ni(Yu,i−μ^), 
#where  ni  is a number of ratings  b  for movie  i .
#
#The larger  λ  is, the more we shrink.  λ  is a tuning parameter, so we can use cross-validation to choose it. We should be using full cross-validation on just the training set, without using the test set until the final assessment.
#We can also use regularization to estimate the user effect. We will now minimize this equation:
#  1N∑u,i(yu,i−μ−bi−bu)2+λ(∑ib2i+∑ub2u) 
#Code
#Please refer to the textbook for an updated version of the code that may contain some corrections.
library(dslabs)
library(tidyverse)
library(caret)
data("movielens")
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
mu_hat <- mean(train_set$rating)
naive_rmse <- RMSE(test_set$rating, mu_hat)
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
user_avgs <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
       left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))

test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>% 
  select(title,  residual) %>% slice(1:10) %>% knitr::kable()

movie_titles <- movielens %>% 
  select(movieId, title) %>%
  distinct()
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()

train_set %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

train_set %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()
lambda <- 3
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

data_frame(original = movie_avgs$b_i, 
           regularlized = movie_reg_avgs$b_i, 
           n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

train_set %>%
  dplyr::count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

train_set %>%
  dplyr::count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()

lambdas <- seq(0, 10, 0.25)
mu <- mean(train_set$rating)
just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()

####Assessment
library(tidyverse)
set.seed(1986, sample.kind="Rounding") # if using R 3.6 or later
n <- round(2^rnorm(1000, 8, 1))

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

schools %>% top_n(10, quality) %>% arrange(desc(quality))

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

#Q1
top10 <- schools %>% arrange(desc(score)) %>% top_n(10)

#Q2
schools %>% summarise(median=median(size))
top10 %>% summarise(median(size))

#Q3
flop10 <- schools %>% arrange(score) %>% top_n(-10)

flop10 %>% summarise(median(size))

#Q4
schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2)
#We can see that the standard error of the score 
#has larger variability when the school is smaller. 
#This is a basic statistical reality we learned in 
#PH125.3x: Data Science: Probability and PH125.4x: 
#Data Science: Inference and Modeling courses! Note also 
#that several of the top 10 schools based on true quality 
#are also in the top 10 schools based on the exam score: 
#schools %>% top_n(10, score) %>% arrange(desc(score)).

#Q5
overall <- mean(sapply(scores, mean))

alpha <- 25
schools5 <- schools %>%
  mutate(score_dev = overall + (score - overall) * size / (size + alpha)) %>%
  arrange(desc(score_dev))
#    mutate(quality_new = score_dev-80)
schools5 %>%
  top_n(10, score_dev)

#Q6
alphas <- seq(10,250)
rmses <- sapply(alphas, function(alpha){
  schools %>%
    mutate(score_dev = overall + (score - overall) * size / (size + alpha)) %>%
    summarize(rmse = sqrt(1/1000 * sum((score_dev-quality)^2))) %>%
    pull(rmse)
})
# solution: other way for same result
# rmses <- sapply(alphas, function(alpha){
#   score_reg <- sapply(scores, function(x) overall+sum(x-overall)/(length(x)+alpha))
#   sqrt(mean((score_reg - schools$quality)^2))
# })
alphas[which.min(rmses)]

#Q7
alpha_best <- 135
schools7 <- schools %>%
  mutate(score_dev = overall + (score - overall) * size / (size + alpha_best)) %>%
  arrange(desc(score_dev)) %>%
  top_n(10, score_dev)
schools7

#Q8
alphas <- seq(10,250)
rmses <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
alphas[which.min(rmses)]

####Matrix Factorization
#Key points
#Our earlier models fail to account for an important source of variation related to the fact that groups of movies and groups of users have similar rating patterns. We can observe these patterns by studying the residuals and converting our data into a matrix where each user gets a row and each movie gets a column:
#  ru,i=yu,i−b^i−b^u, 
#where  yu,i  is the entry in row  u  and column  i .
#We can factorize the matrix of residuals  r  into a vector  p  and vector  q ,  ru,i≈puqi , allowing us to explain more of the variance using a model like this:
#  Yu,i=μ+bi+bu+puqi+ϵi,j 
#Because our example is more complicated, we can use two factors to explain the structure and two sets of coefficients to describe users:
#  Yu,i=μ+bi+bu+pu,1q1,i+pu,2q2,i+ϵi,j 
#To estimate factors using our data instead of constructing them ourselves, we can use principal component analysis (PCA) or singular value decomposition (SVD).
#Code
library(tidyverse)
library(dslabs)
library(matrixStats)
data(movielens)

train_small <- movielens %>% 
  group_by(movieId) %>%
  filter(n() >= 50 | movieId == 3252) %>% ungroup() %>% #3252 is Scent of a Woman used in example
  group_by(userId) %>%
  filter(n() >= 50) %>% ungroup()

y <- train_small %>% 
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>%
  as.matrix()

rownames(y)<- y[,1]
y <- y[,-1]
colnames(y) <- with(movielens, title[match(colnames(y), movieId)])

y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))
y <- sweep(y, 2, colMeans(y, na.rm=TRUE))

m_1 <- "Godfather, The"
m_2 <- "Godfather: Part II, The"
qplot(y[ ,m_1], y[,m_2], xlab = m_1, ylab = m_2)

m_1 <- "Godfather, The"
m_3 <- "Goodfellas"
qplot(y[ ,m_1], y[,m_3], xlab = m_1, ylab = m_3)

m_4 <- "You've Got Mail" 
m_5 <- "Sleepless in Seattle" 
qplot(y[ ,m_4], y[,m_5], xlab = m_4, ylab = m_5)

cor(y[, c(m_1, m_2, m_3, m_4, m_5)], use="pairwise.complete") %>% 
  knitr::kable()

set.seed(1)
options(digits = 2)
Q <- matrix(c(1 , 1, 1, -1, -1), ncol=1)
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5)
P <- matrix(rep(c(2,0,-2), c(3,5,4)), ncol=1)
rownames(P) <- 1:nrow(P)

X <- jitter(P%*%t(Q))
X %>% knitr::kable(align = "c")

cor(X)

t(Q) %>% knitr::kable(aling="c")

P

set.seed(1)
options(digits = 2)
m_6 <- "Scent of a Woman"
Q <- cbind(c(1 , 1, 1, -1, -1, -1), 
           c(1 , 1, -1, -1, -1, 1))
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5, m_6)
P <- cbind(rep(c(2,0,-2), c(3,5,4)), 
           c(-1,1,1,0,0,1,1,1,0,-1,-1,-1))/2
rownames(P) <- 1:nrow(X)

X <- jitter(P%*%t(Q), factor=1)
X %>% knitr::kable(align = "c")

cor(X)

t(Q) %>% knitr::kable(align="c")

P

six_movies <- c(m_1, m_2, m_3, m_4, m_5, m_6)
tmp <- y[,six_movies]
cor(tmp, use="pairwise.complete")

####
#Key points
#You can think of singular value decomposition (SVD) as an algorithm that finds the vectors  p  and  q  that permit us to write the matrix of residuals  r  with  m  rows and  n  columns in the following way:
#  ru,i=pu,1q1,i+pu,2q2,i+...+pu,mqm,i, 
#with the variability of these terms decreasing and the  p ’s uncorrelated to each other.
#SVD also computes the variabilities so that we can know how much of the matrix’s total variability is explained as we add new terms.
#The vectors  q  are called the principal components and the vectors  p  are the user effects. By using principal components analysis (PCA), matrix factorization can capture structure in the data determined by user opinions about movies.
#Code
y[is.na(y)] <- 0
y <- sweep(y, 1, rowMeans(y))
pca <- prcomp(y)

dim(pca$rotation)

dim(pca$x)

plot(pca$sdev)

var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)

library(ggrepel)
pcs <- data.frame(pca$rotation, name = colnames(y))
pcs %>%  ggplot(aes(PC1, PC2)) + geom_point() + 
  geom_text_repel(aes(PC1, PC2, label=name),
                  data = filter(pcs, 
                                PC1 < -0.1 | PC1 > 0.1 | PC2 < -0.075 | PC2 > 0.1))

pcs %>% select(name, PC1) %>% arrange(PC1) %>% slice(1:10)

pcs %>% select(name, PC1) %>% arrange(desc(PC1)) %>% slice(1:10)

pcs %>% select(name, PC2) %>% arrange(PC2) %>% slice(1:10)

pcs %>% select(name, PC2) %>% arrange(desc(PC2)) %>% slice(1:10)

install.packages("recommenderlab")

####Assessment
set.seed(1987, sample.kind="Rounding")
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

#Q1
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)

#The students that test well are at the top of the image and there seem to be three groupings by subject.

#Q2
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#There is correlation among all tests, but higher if the tests are in science and math and even higher within each subject.

#Q3
options(scipen=999)
s <- svd(y)
names(s)

y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

ss_y <- apply(y^2, 2, sum)
ss_yv <- apply((y%*%s$v)^2, 2, sum)
sum(ss_y)

?apply

#Q4
plot(ss_y)
plot(ss_yv)

#Q5
d <- s$d
plot(sqrt(ss_yv), d)
plot(s$d, sqrt(ss_yv))

#or
data.frame(x = sqrt(ss_yv), y = s$d) %>%
  ggplot(aes(x,y)) +
  geom_point()

#Q6
sum(s$d[1:3]^2) / sum(s$d^2)
#The total variability explained can be calculated using the following code: sum(s$d[1:3]^2) / sum(s$d^2). We see that almost 99% of the variability is explained by the first three columns of  YV=UD . So we get the sense that we should be able to explain much of the variability and structure we found while exploring the data with a few columns.

#Q7
identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))

#Q8
plot(s$u[,1]*s$d[1], rowMeans(y))

#Q9
my_image(s$v)
#The first column is very close to being a constant, which implies that the first column of YV is the sum of the rows of Y multiplied by some constant, and is thus proportional to an average. richtig

#Q11
plot(s$u[,2], ylim = c(-0.5, 0.5))
plot(s$v[,2], ylim = c(-0.5, 0.5))
with(s, my_image((u[, 2, drop=FALSE]*d[2]) %*% t(v[, 2, drop=FALSE])))
my_image(resid)

#Q12
resid <- y - with(s,sweep(u[, 1:2], 2, d[1:2], FUN="*") %*% t(v[, 1:2]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#Q13
resid <- y - with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

####Assessment
data("tissue_gene_expression")
dim(tissue_gene_expression$x)

#Q1
pc <- prcomp(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

#Q2
avgs <- rowMeans(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], avg = avgs, 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(avgs, pc_1, color = tissue)) +
  geom_point()
cor(avgs, pc$x[,1])

#Q3
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))

pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

#Q4
data.frame(pc_1 = pc$x[,1], 
           pc_2 = pc$x[,2], 
           pc_3 = pc$x[,3], 
           pc_4 = pc$x[,4], 
           pc_5 = pc$x[,5], 
           pc_6 = pc$x[,6], 
           pc_7 = pc$x[,7], 
           pc_8 = pc$x[,8],
           pc_9 = pc$x[,9],
           pc_10 = pc$x[,10],
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(
             pc_7,
             pc_8,
             pc_9,
             pc_10,
             color = tissue)) + geom_boxplot()

#or
for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}

#Q5
x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

pca <- prcomp(y)

dim(pca$rotation)

dim(pca$x)

plot(pca$sdev)

var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)
var_explained

#or
plot(summary(pc)$importance[3,])

####Assessment
#Q1
d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))

#Q2
hc1 <- hclust(d, method = "complete")
plot(hc1)

#or
h <- hclust(d)
plot(h)

#Q3
library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)
