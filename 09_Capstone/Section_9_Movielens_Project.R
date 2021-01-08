#The submission for the MovieLens project will be three files: a report in the 
#form of an Rmd file, a report in the form of a PDF document knit from your Rmd 
#file, and an R script that generates your predicted movie ratings and calculates
#RMSE. Your grade for the project will be based on two factors:
#  
#  Your report and script (75%)
#The RMSE returned by testing your algorithm on the validation set (the final 
#  hold-out test set) (25%)
#Note that to receive full marks on this project, you may not simply copy code 
#from other courses in the course series and be done with your analysis. Your 
#work on this project needs to build on code that is already provided.#
#
#Please note that once you submit your project, you will not be able to make 
#changes to your submission.
#
#Report and Script (75%)
#Your report and script will be graded by your peers, based on a rubric defined
#by the course staff. Each submission will be graded by three peers and the 
#median grade will be awarded. To receive your grade, you must review and grade 
#the submissions of five of your fellow learners after submitting your own. This 
#will give you the chance to learn from your peers.
#
#Please pay attention to the due dates listed! The project submission is due 
#before the end of the course to allow time for peer grading. Also note that you 
#must grade the reports of your peers by the course close date in order to 
#receive your grade.
#
#RMSE (25%)
#Your movie rating predictions will be compared to the true ratings in the 
#validation set (the final hold-out test set) using RMSE. Be sure that your 
#report includes the RMSE and that your R script outputs the RMSE.
#
#Note that to receive full marks on this project, you may not simply copy 
#code from other courses in the course series and be done with your analysis. 
#Your work on this project needs to build on code that is already provided.
#
#IMPORTANT: Make sure you do NOT use the validation set (the final hold-out 
#test set) to train your algorithm. The validation set (the final hold-out test 
#set) should ONLY be used 
#to test your final algorithm. You should split the edx data into a training and
#test set or use cross-validation.
#
#Honor Code
#You are welcome to discuss your project with others, but all submitted work 
#must be your own. Your participation in this course is governed by the terms 
#of the edX Honor Code. If your project is found to violate the terms of the 
#honor code, you will be unenrolled from the course and be ineligible for a 
#certificate.
#
#Project Due Date
#Submissions for the Movielens project are due one week before course close, 
#on January 8, 2021, at 23:59 UTC. This allows time for peer grading to occur! 
#  Peer grades are due at course close, on January 15, 2021, at 23:59 UTC.
#
#Peer Feedback
#You are strongly encouraged to give your peers thoughtful, specific written 
#feedback in addition to the numerical grades in the rubic. Think about the 
#type of feedback that would help you improve your work and offer that type of 
#feedback to your fellow learners.

#Load libraries
library(tidyverse)
library(caret)
library(dslabs)
library(lubridate)
library(gam)

#Convert timestamp to datetime and then to week
edx <- edx %>% mutate(date = as_datetime(timestamp))
edx <- edx %>% mutate(date = round_date(date, unit = "week"))

#Split edx data into train and test set
set.seed(1, sample.kind = "Rounding")

#Create index to split data (80% train & 20% test)
testIndex <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
trainSet <- edx[-testIndex, ]
testSet <- edx[testIndex, ]

#Make sure UserId and MovieId are existing in train and test set
testSet <- testSet %>% 
  semi_join(trainSet, by = "movieId") %>%
  semi_join(trainSet, by = "userId")

#Define function to compute Root Mean Squared Error (RMSE) based on true outcome and prediction
RMSE <- function(true, predicted){
  sqrt(mean((true - predicted)^2))
}

#Building model with user effect and movie effect
mu <- mean(trainSet$rating) 

movie_avgs <- trainSet %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

user_avgs <- trainSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

genre_avgs <- trainSet %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>% 
  group_by(genres) %>% 
  summarize(b_g = mean(rating - mu - b_i - b_u))

predicted <- testSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres') %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)

RMSE(true=testSet$rating, predicted)

prep_train_set <- trainSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres')

prep_train_set_sample <- sample_n(prep_train_set, 1000)

prep_test_set <- testSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by='genres')

prep_test_set_sample <- sample_n(prep_test_set, 1000)

prep_test_set_sample <- prep_test_set_sample %>% 
  semi_join(prep_train_set_sample, by = "movieId") %>%
  semi_join(prep_train_set_sample, by = "userId")

####Building model with movie and user effect with regularization
#data("movielens")
#test_index <- createDataPartition(y = movielens$rating, times = 1,
#                                  p = 0.2, list = FALSE)
#train_set <- movielens[-test_index,]
#test_set <- movielens[test_index,]
#test_set <- test_set %>% 
#  semi_join(train_set, by = "movieId") %>%
#  semi_join(train_set, by = "userId")

#RMSE <- function(true_ratings, predicted_ratings){
#  sqrt(mean((true_ratings - predicted_ratings)^2))
#}

mu_hat <- mean(trainSet$rating)

naive_rmse <- RMSE(testSet$rating, mu_hat)

rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

mu <- mean(trainSet$rating) 

movie_avgs <- trainSet %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

predicted_ratings <- mu + testSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, testSet$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
user_avgs <- testSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
predicted_ratings <- testSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
model_2_rmse <- RMSE(predicted_ratings, testSet$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))

testSet %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>% 
  select(title, residual) %>% slice(1:10) %>% knitr::kable()

movie_titles <- edx %>% 
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

trainSet %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

trainSet %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

lambda <- 3
mu <- mean(trainSet$rating)
movie_reg_avgs <- trainSet %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

data_frame(original = movie_avgs$b_i, 
           regularlized = movie_reg_avgs$b_i, 
           n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

trainSet %>%
  dplyr::count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

trainSet %>%
  dplyr::count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

predicted_ratings <- testSet %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, testSet$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()

lambdas <- seq(0, 10, 0.25)
mu <- mean(trainSet$rating)
just_the_sum <- trainSet %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- testSet %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, testSet$rating))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(trainSet$rating)
  b_i <- trainSet %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- trainSet %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    testSet %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, testSet$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()


####
