library(tidyverse)
library(dslabs)
data("movielens")
library(caret)
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.2, 
                                  list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

mu <- mean(train_set$rating) 

movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

RMSE(predicted_ratings, test_set$rating)
This gives me a RMSE 0.905

I decided to reversed the order of the effects by redefining the user_avgs and movie_avgs as below and repeated the RMSE calculation

user_avgs <- train_set %>% 
  group_by(userId) %>% 
  summarize(b_i = mean(rating - mu))

movie_avgs <- train_set %>% 
  left_join(user_avgs, by='userId') %>%
  group_by(movieId) %>%
  summarize(b_u = mean(rating - mu - b_i))
And this gives me a RMSE of 0.9112897.