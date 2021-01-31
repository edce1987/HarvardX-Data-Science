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
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
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

####STARTING FROM HERE MY MODEL STARTS

#Load additional libraries
library(lubridate)

##Data Wrangling
#Assumption: The timestamp which indicates when a certain movie was rated by a certain user has an effect on the movie rating.
#Convert timestamp to datetime and then to day.
edxT <- edx %>% mutate(date = round_date(as_datetime(timestamp), unit = "day"))
dim(edxT)

#Assumption: Users that rate more often have more experience and therefore a better judgment which will reduce RMSE.
edxT <- edxT %>% group_by(userId) %>% filter(n() >= 50) %>% ungroup()
dim(edxT)

#Split edx data into train and test set
#Setting seed to 1 to make results reproducible with sample.kind = "Rounding" for R Version > 3.5.
set.seed(1, sample.kind = "Rounding")

#Create index to split data (80% train & 20% test)
testIndex <- createDataPartition(y = edxT$rating, times = 1, p = 0.2, list = FALSE)
trainSet <- edxT[-testIndex, ]
testSet <- edxT[testIndex, ]

#Make sure UserId and MovieId are existing in train and test set
testSet <- testSet %>% 
  semi_join(trainSet, by = "movieId") %>%
  semi_join(trainSet, by = "userId")

#Define function to compute Root Mean Squared Error (RMSE) (with removal of NaNs) based on true outcome and the prediction.
RMSE <- function(true, predicted){
  sqrt(mean((true - predicted)^2, na.rm = TRUE))
}

#Model name: Regularized Model with Movie, User, Time & Genre Effect.
#Find optimal lambda for the model.
#I recommend not running this code since it will take hours.
lambdas <- seq(4.9, 5.15, 0.001)
rmses <- sapply(lambdas, function(l){
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
  predicted_ratings <- testSet %>% #Perform prediction based on feature composition
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_t, by = "date") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_t + b_g) %>%
    .$pred
  return(RMSE(predicted_ratings, testSet$rating))
})

#Find optimal lambda where RMSE is minimized.
lambda <- lambdas[which.min(rmses)]
lambda

#Perform evaluation with optimal lambda.
rmse <- sapply(lambda, function(l){
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
  predicted_ratings <- testSet %>% #Perform prediction based on feature composition
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_t, by = "date") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_t + b_g) %>%
    .$pred
  return(RMSE(predicted_ratings, testSet$rating))
})
rmse

#Model name: Regularized Model with Movie, User, Time & Genre Effect.
#Final evaluation of validation set with final algorithm.
finalRmse <- sapply(lambda, function(l){
  mu <- mean(trainSet$rating) #Feature Composition: Average movie rating mu.
  b_i <- trainSet %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l)) #Feature Composition: Regularized Movie Effect b_i.
  b_u <- trainSet %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>% 
    summarize(b_u = sum(rating - mu - b_i)/(n()+l)) #Feature Composition: Regularized User Effect b_u.
  b_t <- trainSet %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>% 
    group_by(date) %>% 
    summarize(b_t = sum(rating - mu - b_i - b_u)/(n()+l)) #Feature Composition: Regularized Time Effect b_t.
  b_g <- trainSet %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    left_join(b_t, by='date') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u - b_t)/(n()+l)) #Feature Composition: Regularized Genre Effect b_g.
  predicted_ratings <- validation %>% 
    mutate(date = as_datetime(timestamp), date = round_date(date, unit = "day")) %>%  #Required date feature is created.
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_t, by = "date") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_t + b_g) %>% #Perform prediction based on feature composition.
    .$pred #Pull predictions. 
  return(RMSE(predicted_ratings, validation$rating)) #Calculate RMSE.
})

#Print final RMSE.
finalRmse