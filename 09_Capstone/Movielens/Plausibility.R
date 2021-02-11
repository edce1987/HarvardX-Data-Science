l <- 4.8

avg <- mean(trainSet$rating) # Feature Composition: Overall average rating avg.

movie_avg <- trainSet %>%
    group_by(movieId) %>%
    summarize(movie_avg = mean(rating)) # Feature Composition: Movie average for filling NAs later on.
b_movie <- trainSet %>% 
    group_by(movieId) %>%
    summarize(b_movie = sum(rating - avg)/(n()+l)) # Feature Composition: Regularized Movie Effect b_movie.
b_user <- trainSet %>% 
    left_join(b_movie, by="movieId") %>%
    group_by(userId) %>% 
    summarize(b_user = sum(rating - avg - b_movie)/(n()+l)) # Feature Composition: Regularized User Effect b_user.
b_time <- trainSet %>% 
    left_join(b_movie, by='movieId') %>%
    left_join(b_user, by='userId') %>% 
    group_by(date) %>% 
    summarize(b_time = sum(rating - avg - b_movie - b_user)/(n()+l)) # Feature Composition: Regularized Time Effect b_time.
b_genre <- trainSet %>% 
    left_join(b_movie, by='movieId') %>%
    left_join(b_user, by='userId') %>%
    left_join(b_time, by='date') %>%
    group_by(genres) %>%
    summarize(b_genre = sum(rating - avg - b_movie - b_user - b_time)/(n()+l)) # Feature Composition: Regularized Genre Effect b_genre.
predicted_ratings <- validation %>% 
    mutate(date = as_datetime(timestamp), date = round_date(date, unit = "day")) %>% # Feature engineering of date.
    left_join(movie_avg, by = "movieId") %>% # Left join of the created movie avg.
    left_join(b_movie, by = "movieId") %>%
    left_join(b_user, by = "userId") %>%
    left_join(b_time, by = "date") %>%
    left_join(b_genre, by = "genres") %>%
    mutate(pred = avg + b_movie + b_user + b_time + b_genre) %>% # Perform prediction based on feature composition.
    mutate(pred = ifelse(is.na(pred), movie_avg, pred)) %>% # Replace NAs with movie average. Rationale: Due to our data restrictions in the data wrangling part (only users with > 15 ratings) and selected biases (esp. time and genre), some NAs are still present in the results since there is no value available for all movie, user, genre, time combinations. Hence, we replace those NAs with either 1. movie average (more precise) or 2. with the overall average in case the movie average is not available.
    mutate(pred = ifelse(is.na(pred), avg, pred)) %>% # Replace remaining NAs with overall average.
    mutate(pred = ifelse(pred < 0, 0.5, ifelse(pred > 5, 5, pred))) # Computed predictions are numerical and can be out of the "allowed" rating range from 0.5 to 5.0. To avoid penalties in the RMSE, we trim these values to the nearest allowed rating.
     

RMSE(predicted_ratings$pred, validation$rating)


#View(predicted_ratings)
length(predicted_ratings$pred)
sum(is.na(predicted_ratings$pred))

length(validation$rating)
sum(is.na(validation$rating))

View(predicted_ratings)