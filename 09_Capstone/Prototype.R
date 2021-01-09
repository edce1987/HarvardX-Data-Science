l <- lambda

mu <- mean(trainSet$rating) #Feature Composition: Average movie rating mu.

b_i <- trainSet %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l)) #Feature Composition: Regularized Movie Effect b_i.
dim(b_i)

b_u <- trainSet %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>% 
  summarize(b_u = sum(rating - mu - b_i)/(n()+l)) #Feature Composition: Regularized User Effect b_u.
dim(b_u)

b_t <- trainSet %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>% 
  group_by(date) %>% 
  summarize(b_t = sum(rating - mu - b_i - b_u)/(n()+l)) #Feature Composition: Regularized Time Effect b_t.
dim(b_t)

b_g <- trainSet %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by='userId') %>%
  left_join(b_t, by='date') %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u - b_t)/(n()+l)) #Feature Composition: Regularized Genre Effect b_g.
dim(b_g)

predicted_ratings <- validation %>% 
  mutate(date = as_datetime(timestamp), date = round_date(date, unit = "day")) %>%  #Required date feature is created.
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_t, by = "date") %>%
  left_join(b_g, by = "genres") %>%
  mutate(pred = mu + b_i + b_u + b_t + b_g) %>% #Perform prediction based on feature composition.
  .$pred 

RMSE(predicted_ratings, validation$rating)

length(predicted_ratings)
sum(is.na(predicted_ratings))
mean(is.na(predicted_ratings))
