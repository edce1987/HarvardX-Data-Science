l <- lambda
#Final evaluation of validation set with final algorithm.
finalRmse <- sapply(lambda, function(l){
  mu <- mean(trainSet$rating) #Feature Composition: Average movie rating mu
  b_i <- trainSet %>% #Feature Composition: Regularized Movie Effect b_i
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- trainSet %>% #Feature Composition: Regularized User Effect b_u
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>% 
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))
  b_t <- trainSet %>% #Feature Composition: Regularized Time Effect b_t
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>% 
    group_by(date) %>% 
    summarize(b_t = sum(rating - mu - b_i - b_u)/(n()+l))
  b_g <- trainSet %>% #Feature Composition: Regularized Genre Effect b_g
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_t, by='date') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u - b_t)/(n()+l))
  predicted_ratings <- validation %>% #Perform prediction based on feature composition
    mutate(date = as_datetime(timestamp), date = round_date(date, unit = "day")) %>%  #Required feature is created.
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_t, by = "date") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_t + b_g) %>%
    .$pred
  return(RMSE(predicted_ratings, validation$rating))
})

finalRmse

All_rmse <- append(All_rmse, finalRmse)
All_rmse