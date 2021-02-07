##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
#                                           title = as.character(title),
#                                           genres = as.character(genres))

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#### My Capstone project start from here.                ####
#### MovieLens Movie Recommendation System by Edin Ceman ####

# Load additional libraries
library(lubridate)

## Data Wrangling

#Check if we have NAs in the edx dataset:
sapply(edx, function(x) sum(is.na(x)))

# Create additional features and prepare edx data set.
# Assumption: The timestamp which indicates when a certain movie was rated by a certain user has an effect on the movie rating.
# Convert timestamp to datetime and then to day to have a granular view.
edxT <- edx %>% mutate(date = round_date(as_datetime(timestamp), unit = "day"))
dim(edxT)

# Assumption: Users that rate more often have more experience and therefore a better judgment which will reduce RMSE. Also users with more data points (ratings) will enable us to predict their preferences more accurately.
edxT <- edxT %>% group_by(userId) %>% filter(n() >= 15) %>% ungroup()
dim(edxT)

# Split edx data into train and test set
# Setting seed to 1 to make results reproducible with sample.kind = "Rounding" for R Version > 3.5.
set.seed(1, sample.kind = "Rounding")

# Create index to split data (80% train & 20% test)
testIndex <- createDataPartition(y = edxT$rating, times = 1, p = 0.2, list = FALSE)
trainSet <- edxT[-testIndex, ]
testSet <- edxT[testIndex, ]

# Make sure UserId and MovieId are existing in train and test set by applying semi_join.
testSet <- testSet %>% 
  semi_join(trainSet, by = "movieId") %>%
  semi_join(trainSet, by = "userId")

# Define function to compute Root Mean Squared Error (RMSE) (with removal of NAs due to robustness, NAs are replaced at a later step). 
# Based on true outcome and the prediction.
RMSE <- function(true, predicted){
  sqrt(mean((true - predicted)^2, na.rm = TRUE))
}

## Modeling

# Name: Regularized Model with Movie, User, Time & Genre Effect.
# We want to capture different "biases" in the training dataset to identify the effects that influence the resulting rating. 
# We also want to use a regularized model to avoid overfitting. 
# Hence, we need to find the optimal "penalty parameter" lambda for the regularized model. I have narrowed the range of the optimal parameter to avoid excessive computing time. 
# From my trials I narrowed down the optimal area somewhere between 5.20 to 5.40 in 0.001 steps which still equals 200 iterations.
# I recommend not running this code since it will take very long depending on your hardware. 
lambdas <- seq(4.80, 4.95, 0.001)

# Training & evaluation of the regularized model using the train & test set. 
# The goal of the model is to capture the different biases (see below) in relation to the average movie rating to predict a movie rating for a certain user.
rmses <- sapply(lambdas, function(l){
  avg <- mean(trainSet$rating) # Feature Composition: Overall average rating: avg.
  movie_avg <- trainSet %>% # Feature Composition: Movie average for filling NAs later on.
    group_by(movieId) %>%
    summarize(movie_avg = mean(rating)) 
  b_movie <- trainSet %>% # Feature Composition: Regularized Movie bias: b_movie.
    group_by(movieId) %>%
    summarize(b_movie = sum(rating - avg)/(n()+l))
  b_user <- trainSet %>% # Feature Composition: Regularized User bias: b_user.
    left_join(b_movie, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_user = sum(rating - avg - b_movie)/(n()+l))
  b_time <- trainSet %>% # Feature Composition: Regularized Time bias: b_time.
    left_join(b_movie, by='movieId') %>%
    left_join(b_user, by='userId') %>% 
    group_by(date) %>% 
    summarize(b_time = sum(rating - avg - b_movie - b_user)/(n()+l))
  b_genre <- trainSet %>% # Feature Composition: Regularized Genre bias: b_genre.
    left_join(b_movie, by='movieId') %>%
    left_join(b_user, by='userId') %>%
    left_join(b_time, by='date') %>%
    group_by(genres) %>%
    summarize(b_genre = sum(rating - avg - b_movie - b_user - b_time)/(n()+l))
  predicted_ratings <- testSet %>% # Perform prediction based on feature composition.
    left_join(movie_avg, by = "movieId") %>% # Left join of the created biases and features to the test set.
    left_join(b_movie, by = "movieId") %>%
    left_join(b_user, by = "userId") %>%
    left_join(b_time, by = "date") %>%
    left_join(b_genre, by = "genres") %>%
    mutate(pred = avg + b_movie + b_user + b_time + b_genre) %>% # Perform prediction based on feature composition.
    mutate(pred = ifelse(is.na(pred), movie_avg, pred)) %>% # Replace NAs with movie average. Rationale: Due to our data restrictions in the data wrangling part (only users with > 15 ratings) and selected biases (esp. time and genre), some NAs are still present in the results since there is no value available for all movie, user, genre, time combinations. Hence, we replace those NAs with either 1. movie average (more precise) or 2. with the overall average in case the movie average is not available.
    mutate(pred = ifelse(is.na(pred), avg, pred)) %>% # Replace remaining NAs with overall average.
    mutate(pred = ifelse(pred < 0, 0.5, ifelse(pred > 5, 5, pred))) %>% # Computed predictions are numerical and can be out of the "allowed" rating range from 0.5 to 5.0. To avoid penalties in the RMSE, we trim these values to the nearest allowed rating.
    .$pred
  return(RMSE(predicted_ratings, testSet$rating))
})

# Find optimal lambda where the RMSE is minimized.
lambda <- lambdas[which.min(rmses)]

# Print optimal lambda
lambda

# Perform evaluation on test & train set with optimal lambda only.
rmse <- sapply(lambda, function(l){
  avg <- mean(trainSet$rating) # Feature Composition: Overall average rating: avg.
  movie_avg <- trainSet %>% # Feature Composition: Movie average for filling NAs later on.
    group_by(movieId) %>%
    summarize(movie_avg = mean(rating)) 
  b_movie <- trainSet %>% # Feature Composition: Regularized Movie bias: b_movie.
    group_by(movieId) %>%
    summarize(b_movie = sum(rating - avg)/(n()+l))
  b_user <- trainSet %>% # Feature Composition: Regularized User bias: b_user.
    left_join(b_movie, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_user = sum(rating - avg - b_movie)/(n()+l))
  b_time <- trainSet %>% # Feature Composition: Regularized Time bias: b_time.
    left_join(b_movie, by='movieId') %>%
    left_join(b_user, by='userId') %>% 
    group_by(date) %>% 
    summarize(b_time = sum(rating - avg - b_movie - b_user)/(n()+l))
  b_genre <- trainSet %>% # Feature Composition: Regularized Genre bias: b_genre.
    left_join(b_movie, by='movieId') %>%
    left_join(b_user, by='userId') %>%
    left_join(b_time, by='date') %>%
    group_by(genres) %>%
    summarize(b_genre = sum(rating - avg - b_movie - b_user - b_time)/(n()+l))
  predicted_ratings <- testSet %>% # Perform prediction based on feature composition.
    left_join(movie_avg, by = "movieId") %>% # Left join of the created biases and features to the test set.
    left_join(b_movie, by = "movieId") %>%
    left_join(b_user, by = "userId") %>%
    left_join(b_time, by = "date") %>%
    left_join(b_genre, by = "genres") %>%
    mutate(pred = avg + b_movie + b_user + b_time + b_genre) %>% # Perform prediction based on feature composition.
    mutate(pred = ifelse(is.na(pred), movie_avg, pred)) %>% # Replace NAs with movie average. Rationale: Due to our data restrictions in the data wrangling part (only users with > 15 ratings) and selected biases (esp. time and genre), some NAs are still present in the results since there is no value available for all movie, user, genre, time combinations. Hence, we replace those NAs with either 1. movie average (more precise) or 2. with the overall average in case the movie average is not available.
    mutate(pred = ifelse(is.na(pred), avg, pred)) %>% # Replace remaining NAs with overall average.
    mutate(pred = ifelse(pred < 0, 0.5, ifelse(pred > 5, 5, pred))) %>% # Computed predictions are numerical and can be out of the "allowed" rating range from 0.5 to 5.0. To avoid penalties in the RMSE, we trim these values to the nearest allowed rating.
    .$pred
  return(RMSE(predicted_ratings, testSet$rating))
})

# Print RMSE.
rmse

#### Final prediction of ratings using the validation set.                ####
#### Model name: Regularized Model with Movie, User, Time & Genre Effect. ####
finalRmse <- sapply(lambda, function(l){
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
    mutate(pred = ifelse(pred < 0, 0.5, ifelse(pred > 5, 5, pred))) %>% # Computed predictions are numerical and can be out of the "allowed" rating range from 0.5 to 5.0. To avoid penalties in the RMSE, we trim these values to the nearest allowed rating.
    .$pred # Pull predictions. 
  return(RMSE(predicted_ratings, validation$rating)) # Calculate RMSE.
})

# Print final RMSE.
finalRmse