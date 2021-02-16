
# Introduction
## Project goals and key steps
## Data download
# The following code was used to download the MovieLens data and generate both 
# training and test datasets and was provided by the [HarvardX PH125.9x
# Data Science Capstone course 
# https://learning.edx.org/course/course-v1:HarvardX+PH125.9x+2T2020/block-v1:HarvardX+PH125.9x+2T2020+type@sequential+block@e8800e37aa444297a3a2f35bf84ce452/block-v1:HarvardX+PH125.9x+2T2020+type@vertical+block@e9abcdd945b1416098a15fc95807b5db

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

# With the data partition, we end up with a train set (edx) with 90% of the 
# observations and a validation set with the remaining 10% after applying the 
# createDataPartition() function on the movielens dataset:

nrow(edx)
nrow(validation)
# A first look at the data
head(edx, 6)

## Data exploration
# Let's download some libraries that will be used later.
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
library(dplyr)
library(lubridate)
# The data shown in the data download section gives us a few ideas to improve the use
# of it in our recommendation system:
# - The "timestamp" column is the date and time when the user did the rating. It 
#   would be useful to extract the year when the rating was done in order to use 
#   it as a feature/predictor in combination with the release year. The rest of the 
#   timestamp (month, day and time) could be used for a further analysis, but we will
#   skip it in this work.
# - The "title" column contains the release year of the movie. It will be useful to 
#   extract it and have it separate to use as an additional feature.
# - The "genres" column contains one or more different genres applicable to a 
#   particular movie. To make better use of this information, it will be separated in
#   individual genres to facilitate the grouping of movie ratings for further analysis.

### Extract the year when user did the review
# The following code will take data in the timestamp column, convert into a Date & 
# Time format using the as_datetime() function in the lubridate package and then 
# extract the year from it and put it into a new column (rate_year). We do it for 
# both edx and validation sets.
edx <- mutate(edx, rate_year = year(as_datetime(timestamp)))
validation <- mutate(validation, rate_year = year(as_datetime(timestamp)))

# Let's check the years obtained are consistent and there is no weird entry (e.g.: 
# before 1900 or beyond current year of 2020). The outcome shows the range of years 
# users did their ratings is consistent and were done between 1995 and 2009. No need 
# to verify for the test set, as all moviIds in it are presen in the train set.
range(edx$rate_year)

### Extract the movie release year
# The following code will parse each string in the "title" column and extract from it
# the YYYY placed within the parenthesis, convert it to a number and put it into a 
# new column, "release_year".
edx <- edx %>% mutate(release_year = as.numeric(str_sub(title,-5,-2)))
validation <- validation %>% mutate(release_year =  as.numeric(str_sub(title,-5,-2)))

# Let's check the years obtained are consistent and there is no weird entry (e.g.: 
# before 1900 or beyond current year of 2020). The outcome shows the range of movie 
# release year is consistent and were released between 1915 and 2008. No need to 
# verify for the test set, as all moviIds in it are presen in the train set.
range(edx$release_year)

# Looking now at our train set (edx) we see the data we have extracted.
head(edx)

### Separate data from genres column into individual entries
# In order to do this, we will be creating new additional train and test sets, as 
# there will be quite an increase in the number of final rows. And for future 
# calculations when genres data is not involved, it is worth  to use the shorter 
# datasets as the execution time of our calculations will be much shorter.
edx_split_genre  <- edx %>% separate_rows(genres, sep = "\\|")
validation_split_genre <- validation %>% separate_rows(genres, sep = "\\|")

# The number of rows in the train set has gone from about 9M rows to more than 23M, 
# about 2.5 times more.
nrow(edx_split_genre)

# Similar thing happens for the test set, going from about 1M rows to 2.6M, 2.6 
# times more
nrow(validation_split_genre)

# Having a look a the train set, we can see the genres column with individual entries:
head(edx_split_genre)

###  Create train and test sets from edx dataset
# We will split the edx data set into train and test sets and keep the validation 
# set obtained previously for final prediction and evaluation of the best of the 
# models.
set.seed(2021, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set
edx_test <- temp %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

#Add rows removed from test set back into train set
removed <- anti_join(temp, edx_test)
edx_train <- rbind(edx_train, removed)
nrow(edx)
nrow(edx_train)
nrow(edx_test)

# Separate genres column into individual values
edx_train_split_genre  <- edx_train %>% separate_rows(genres, sep = "\\|")
edx_test_split_genre <- edx_test %>% separate_rows(genres, sep = "\\|")

# Methods & Analysis
## Data analysis
# A quick look at our edx dataset:
glimpse(edx)

# Let's see some measurements for each column, obviously not character type columns.

# Let's look at how many different users, movies, genres, rate years and release 
# years we have in our data: 
edx %>% summarize(n_users = n_distinct(userId), n_movies = n_distinct(movieId), n_ratings = n_distinct(rating), n_genres = n_distinct(genres), n_rate_years = n_distinct(rate_year), n_release_years = n_distinct(release_year))
edx_split_genre %>% summarize(n_users = n_distinct(userId), n_movies = n_distinct(movieId), n_ratings = n_distinct(rating), n_genres = n_distinct(genres), n_rate_years = n_distinct(rate_year), n_release_years = n_distinct(release_year))

# To be noticed that splitting the genres column into individual values we have 
# reduced the numer of different genres from 797 to 20.

# Looking at the different ratings users have given to movies, we can see they are 
# 10 different values ranging from 0.5 to 10 and can be given by seq(0.5, 5, 0.5). 
# This means that our rating outcome is of categorical type with 10 different classes.

# Also, we have seen in the Quiz within this Capstone course that:
# - The five most given ratings in order from most to least are: 4, 3, 5, 3.5, 2 
# - In general, half star ratings are less common than whole star ratings 

# We will confirm that in a later plot. Let's have a look at the movies with highest
# number of user ratings. As seen by the titles, they are all blockbusters watched by
# a big audience.
edx %>% group_by(movieId, title) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  head()

# On the opposite side, if we check for the movies with the lowest number of user 
# ratings which we can see by the title they all seem to be artsy movies seeing by 
# not many people. In our case, the movies have just a single rating.
edx %>% group_by(movieId, title) %>% 
  summarize(n = n(), genres = genres) %>% 
  arrange(n) %>% 
  head()

### Ratings Distribution
# Let's plot the distribution of ratings without taking into consideration any of 
# the predictors
edx %>% ggplot(aes(rating)) +
  geom_histogram(bins = 20) +
  ggtitle("Rating Distribution")

# We can confirm from the plot above that round ratings are the ones having most 
# entries as indicated earlier. In particular, the 3.0 with more than 2M entries 
# and the 4.0 with more than 2.5M entries.

### Distribution of movies versus their average rating
# Let's examine the distribution of movies based on their rating average. We are 
# only considering those movies with more than 100 ratings. The overal rating across
# all movies already seen in previous section with the summary function is 3.512 
# (mean) and a median value of 4.000.
edx %>% 
  group_by(movieId) %>% 
  summarize(average_rating = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(average_rating)) + 
  geom_histogram(bins = 30, color = "black") +
  xlab("Movie average rating") +
  ylab("Number of movies") +
  ggtitle("Distribution of movies versus their average rating")

# Another way to see movies versus their rating average, using all points and 
# geom_point() function rather than a histogram, would be the following plot:
edx %>%
  group_by(movieId) %>%
  summarize(average_rating = mean(rating)) %>% 
  ggplot(aes(movieId, average_rating)) +
  geom_point(alpha = .50, color = "black") +
  geom_smooth() +
  ggtitle("Distribution of movies versus their average rating")

# To be noticed there are some ranges of movieIds for which the 10M MovieLens dataset
# does not have any entry. The geom_smooth() function shows us almost a horizontal 
# line, with a bit of wiggle at the beginning and below the global average of 3.512.

### Distribution of movies versus their number of ratings
# A different way to examine the movies is plotting them against their number of 
# ratings.
edx %>%
  group_by(movieId) %>% 
  summarize(n = n()) %>%
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  xlab("number of ratings") +
  ylab("number of movies") +
  ggtitle("Distribution of Movies versus number of ratings")

# We can see in the histogram there are many movies with very few ratings (e.g.: less
# than 10) that will have an adverse effect in our prediction model and will have to 
# be taken into account This indicates there is an effect of the movie feature over 
# the rating outcome


### Distribution of users versus their average rating
# Let's examine the distribution users against their rating average. We are only 
# considering those users who have rated more than 100 movies.
edx %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black") +
  xlab("User average rating") +
  ylab("Number of users") +
  ggtitle("Distribution of users versus their average rating")

# The histogram shows quite a uniform distribution around the global rating average. 
# But we will check in the next section for a rating bias due to user.

# Another way to see users versus their average ratings, using all points and 
# geom_point()) function rather than as a histogram, would be the following plot:
edx %>%
  group_by(userId) %>%
  summarize(average_rating = mean(rating)) %>% 
  ggplot(aes(userId, average_rating)) +
  geom_point(alpha = .50, color = "black") +
  geom_smooth() +
  ggtitle("Distribution of users versus their average rating")

# The plot above shows user ratings are quite constrained within the band of 3.0 to 
# 4.0, and the geom_smooth() line is just a horizontal line with "y" value the global
# average of ratings.

### Distribution of users versus their number of ratings
# A different way to examine the users is plotting them against their number of 
# ratings.
edx %>%
  group_by(userId) %>% 
  summarize(n = n()) %>%
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  xlab("Number of ratings") +
  ylab("Number of users") +
  ggtitle("Users versus their number of ratings")

# The histogram above shows how there are users quite active (e.g.: UserId 59269 with
# 6616 ratings) and users not very active (e.g.: least active users have 10, 12, 13
# or 14 ratings). This means there might be also a user effect on rating that will be
# later evaluated.

### Ratings versus genres
# Let's examine the distribution of the average rating of each genre. The number of 
# different genres is 20 as can be seen below.
n_distinct(edx_split_genre$genres)

# The plot would be:
edx_split_genre %>% 
  group_by(genres) %>% 
  summarize(b_g = mean(rating)) %>% 
  ggplot(aes(b_g)) + 
  geom_histogram(bins = 30, color = "black") +
  xlab("Genre average rating") +
  ylab("Number of genres") +
  ggtitle("Distribution of genres versus rating")

# To be reminded for the picture above that the total number of different genres is 
# 20. The plot shows that almost half of the genres (9 out of 20) fall within the 
# average rating interval of 3.4-3.6. 
edx_split_genre %>% 
  group_by(genres) %>% 
  summarize(b_g = mean(rating)) %>% 
  arrange(desc(b_g))

# The Horror genre is the one with the lowest average rating (3.27) and Film-Noir 
# the one with the highest one (4.01). Obviously, Film-Noir movies are not the kind 
# of ones most watched and, by extension, rated by not as many people as blockbusters.
# This means there is a bias associated with the genre that we need to take into
# account.

### Genres trend versus release year
# Let's examine the trend of movie genres versus the release year of the movie. As 
# there are 20 different genres, in order to avoid a crowdy graph, we will consider 
# only the 5 genres with the most entries.
edx_split_genre %>%
  group_by(genres) %>% 
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  head(5)

# We now know the target genres are Drama, Comedy, Action, Thriller and Adventure. 
# The plot would be then:
edx_split_genre %>% 
  filter(genres %in% c("Drama", "Comedy", "Action", "Thriller", "Adventure")) %>%
  group_by(genres, release_year) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x = release_year, y = n)) +
  geom_line(aes(color=genres)) +
  #scale_y_log10() 
  xlab("release year") +
  ylab("Genres") +
  ggtitle("Distribution of genres versus release year")

# The plot shows that genre preference changes during time, and that could have an 
# effect on the final rating of movies. For example, prior to 1990, the most ratings 
# where for the Comedy genre, but after 1990, user taste changes and Drama is the 
# genre rated the most. This means the release year has some influence on the genres 
# and, as genre affect rating, release year would also have an effect on the rating 
# that we have to take into account in our model.

### Ratings versus rate year
# Let's examine the distribution of the ratings versus the rating year.
edx %>% group_by(rate_year) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(rate_year, rating)) +
  geom_point() +
  geom_smooth() +
  xlab("rate year") +
  ylab("rating average") +
  ggtitle("Distribution of rating average versus rate year")

# The plot above shows that the average rating of movies during a particular year 
# has decreased since 1995 till almost 2005 when it slowly picked up. Is it because 
# user taste changed with the years? Maybe because new movies coming out are not as 
# good? This makes the rate_year a good candidate to have some effect on the rating.

### Ratings versus release year
# Let's examine the distribution of the ratings versus the release year of movies
edx %>% group_by(release_year) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(release_year, rating)) +
  geom_point() +
  geom_smooth() +
  xlab("release year") +
  ylab("rating average") +
  ggtitle("Distribution of release year versus average rating")

# The plot above shows movies from the 40's and 50's have got the highest ratings 
# from users and, since then, the average rating of movies has decreased. Is it 
# because in those two decades were better than movies before and after? Somehow, 
# it seems like the release year has some effect on the rating, so we have to take 
# into account in our prediction model.


## Recomendation Models
# The purpose of the movie recommendation system we are creating is to generate a 
# set of predicted movie ratings against the validation set that minimizes as much 
# as possible the residual mean squared error (RMSE).

# As the RMSE is something we will be calculating quite a few times while training 
# our algorithm, it is worth to create a function for it:
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Based on the different plots analyzed in previous section, we have seen there might
# be some potential biases affecting the rating due the movie, the user, the genre, 
# the rate year and the release year.

# We will start following the line in the course text book with a model that includes
# the bias effect of the different features/predictors as well as introducing some 
# penalty for some of them. Then, we will go on with the recommendation systems based 
# on collaborative filtering (CF).

### Model 1: regularized bias effect of the predictors
# We will take into account the bias introduced by each of the predictors (movie, 
# user, genre, release year and rate year. We will train our model to find the lambda
# value that minimizes the RMSE value. For this purpose, we use the train set, 
# edx_train_split_genre, that has individual genre entries.

# Note: the lambda calculation took about 30 minutes to complete on a laptop with 
# Windows10 OS and 16GB of RAM using most of the time the full memory and with no 
# other application running.
mu <- mean(edx_train$rating)
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  
  b_i <- edx_train_split_genre %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx_train_split_genre %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_rly <- edx_train_split_genre %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(release_year) %>%
    summarize(b_rly = sum(rating - b_u - b_i - mu)/(n()+l))
  
  b_g <- edx_train_split_genre %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_rly, by="release_year") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_rly - b_u - b_i - mu)/(n()+l))
  
  b_rty <- edx_train_split_genre %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_rly, by="release_year") %>%
    left_join(b_g, by="genres") %>%
    group_by(rate_year) %>%
    summarize(b_rty = sum(rating - b_g - b_rly - b_u - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    edx_train_split_genre %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_rly, by = "release_year") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_rty, by = "rate_year") %>%
    mutate(pred = mu + b_i + b_u + b_rly + b_g) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, edx_train_split_genre$rating))
})

# Plotting lambdas versus rmses:
qplot(lambdas, rmses)

# The minimum value of the RMSE obtained using the train set is:
min(rmses)

# The lambda that minimizes the RMSE is:
lambda <- lambdas[which.min(rmses)]
lambda

# Compute regularized estimates of b_i using lambda
movie_avgs_reg <- edx_train_split_genre %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())

# Compute regularized estimates of b_u using lambda
user_avgs_reg <- edx_train_split_genre %>% 
  left_join(movie_avgs_reg, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+lambda), n_u = n())

# Compute regularized estimates of b_rly using lambda
rel_year_avgs_reg <- edx_train_split_genre %>% 
  left_join(movie_avgs_reg, by='movieId') %>%
  left_join(user_avgs_reg, by='userId') %>%
  group_by(release_year) %>%
  summarize(b_rly = sum(rating - mu - b_i - b_u)/(n()+lambda), n_y = n())

# Compute regularized estimates of b_g using lambda
genre_avgs_reg <- edx_train_split_genre %>% 
  left_join(movie_avgs_reg, by='movieId') %>%
  left_join(user_avgs_reg, by='userId') %>%
  left_join(rel_year_avgs_reg, by='release_year') %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u - b_rly)/(n()+lambda), n_y = n())

# Compute regularized estimates of b_rty using lambda
rt_year_avgs_reg <- edx_train_split_genre %>% 
  left_join(movie_avgs_reg, by='movieId') %>%
  left_join(user_avgs_reg, by='userId') %>%
  left_join(rel_year_avgs_reg, by='release_year') %>%
  left_join(genre_avgs_reg, by='genres') %>%
  group_by(rate_year) %>%
  summarize(b_rty = sum(rating - mu - b_i - b_u - b_rly - b_g)/(n()+lambda), n_y = n())

# Predict ratings for our validation set:
predicted_ratings_reg <- edx_test_split_genre %>% 
  left_join(movie_avgs_reg, by='movieId') %>%
  left_join(user_avgs_reg, by='userId') %>%
  left_join(rel_year_avgs_reg, by='release_year') %>%
  left_join(genre_avgs_reg, by='genres') %>%
  left_join(rt_year_avgs_reg, by='rate_year') %>%
  mutate(pred = mu + b_i + b_u + b_rly + b_g + b_rty) %>% 
  .$pred

# Calculate RMSE against validation set with generated prediction:
model_1_rmse <- RMSE(edx_test_split_genre$rating,predicted_ratings_reg)
model_1_rmse

errors <- tibble(algorithm = "POPULAR", method = "split", RMSE = 0.9410653)
errors

# Note: the RMSE value obtained against the training set is lower than the one 
# obtained against the test set due to overtraining.

### Collaborative Filtering (CF) Models - General Properties
# We will be using the **recommender** package to test most of the CF models applied
# in the following sections. It has to be mentioned the recommenderlab is not a 
# recommender algorithm itself, but it provides the infrastructure to develop and 
# test recommender algorithms for rating data. We will be applying some of the basic 
# recommender algorithms this package comes with (e.g.: POPULAR, UBCF, IBCF, SVD and 
# SVDF). It also allows the user to develop and use his/her own algorithms in the 
# framework via a simple registration procedure, but that is out of the scope of this
# project.

# We will evaluate them using the RMSE, although they provide also other error 
# measures like the MSE (Mean Square Error) and MAE (Mean Average Error) as it will 
# be seen in later sections.

# First we have to install the recommenderlab package:
if(!"recommenderlab" %in% rownames(installed.packages())){install.packages("recommenderlab")}
library("recommenderlab")

# To see the CF algorithms included in the recommender package:
recommendation_model = recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)

# We will also install the e1071 package, as it will be needed for some of the 
# functions of the algorithsm included in the recommenderlab package. It contains 
# Functions for latent class analysis, short time Fourier transform, fuzzy clustering, support vector machines, shortest path computation, bagged clustering, naive Bayes classifier, etc.
if(!"e1071" %in% rownames(installed.packages())){install.packages("e1071")}
library(e1071)
if(!"devtools" %in% rownames(installed.packages())){install.packages("devtools")}
library(devtools)

# Although the plan was to use the edx dataset (9M rows) for the evaluation of the 
# CF models, it was too big for some of them (e.g.: UBCF and IBCF), and either 
# creating the evaluation schema originated from the rating matrix obtained from edx
# dataset or calculating predictions for the models never completed to execute. 
# Because of this, to initially evaluate the CF models, we have created a subset 
# (edx_cf) sized in a 10% of the original edx dataset. I had previously verified the 
# CF models used worked fine with the 1M dataset from MovieLens available at 
# https://grouplens.org/datasets/movielens/ and the zip file for it directly 
# downloaded from http://files.grouplens.org/datasets/movielens/ml-1m.zip
set.seed(2021, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
cf_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_cf <- edx[cf_index,]
nrow(edx_cf)

#### Rating Matrix
# A rating matrix, of realRatingMatrix type, is the basic input for all recommender 
# CF models. Each row represents a user and each column a movie, and element ij in 
# the matrix would be the rating of user "i" for movie "j". So, before getting into 
# the CF algorithms, let's obtain the rating matrix from our edx_cf dataset for 
# future use. Most CF algorithms take as input a rating matrix and split it into 
# train and test. Because of this, there is no need at this point to partition the 
# edx_cf dataset into train and test sets as we did for bias model and we will
# generate our rating matrix directly from the edx_cf dataset, leaving the validation
# set for final evaluation of the best model.

# Let's first create the sparse matrix
sparse_ratings <- sparseMatrix(i = edx_cf$userId, j = edx_cf$movieId, x = edx_cf$rating, 
                               dims = c(max(edx_cf$userId), max(edx_cf$movieId)),  
                               dimnames = list(paste("u", seq(max(edx_cf$userId)), sep = ""), 
                                               paste("m", seq(max(edx_cf$movieId)), sep = "")))
sparse_ratings

str(sparse_ratings)

# Let's have a look at its first 10 rows and columns
sparse_ratings[1:10,1:10]

# Not a very good example the first 10 rows and columns, as there is not a single 
# rating in them. Normally, there should be a few ratings among most empty values. 
# Let's convert now the sparse matrix into a realRatingMatrix:
real_ratings <- new("realRatingMatrix", data = sparse_ratings)
real_ratings

# From the rating matrix we can easily plot histograms for different measurements: 
# The *breaks* parameter is the number of cells for the histogram

# Histograms of ratings
hist(getRatings(real_ratings), breaks=100)
# Histogram of normalized ratings using row centering
# Histogram of normalized ratings using Z-score normalization
hist(getRatings(normalize(real_ratings, method="Z-score")), breaks=100)
# Histogram of the number of rated items per user
hist(rowCounts(real_ratings), breaks=50)
# Histogram of number of movies per average rating
hist(colMeans(real_ratings), breaks=20)

#### Similarity Matrix
# Recommending movies is dependent on creating a relationship of similarity between 
# pairs of users. With the help of the recommenderlab, we can compute similarities 
# using various operators like cosine, pearson as well as jaccard. To create the 
# similarity matrix of the first 4 users in our rating matrix
similaritY_mat = similarity(real_ratings[1:4,],method = "cosine",which="users")
as.matrix(similaritY_mat)

# Each cell in this matrix represents the similarity that is shared between the two 
# users. And showing it as an image:
image(as.matrix(similaritY_mat),main="Users Similarities")

# We can also describe the similarity that is shared between the movies:
movie_similarity = similarity(real_ratings[,1:4],method="cosine",which="items")
as.matrix(movie_similarity)

# And also show it as an image.
image(as.matrix(movie_similarity), main="Movies Similarities")

#### HeatMap of Movie Ratings
# We can also see an image of the realRatingMatrix that is called a heatmap. This 
# shows users on the rows and movies on the columns and each intersection pixel has
# the rating value in a scale of gray color. For example, a heatmap containing the 
# first 25 rows and 25 columns:
image(real_ratings[1:25,1:25],axes=FALSE,main="HeatMap of first 25  rows and 25 columns")

# We can see there are only 5 ratings among the 25x25 (= 625) pixels, which indicates
# a high level of sparsity (very few ratings among the 25x25 matrix), although the 
# sparsity is even higher for the whole rating matrix (900007 / (71567 * 65133) = 0.00019).

# For finding useful data in our dataset, we can take into account only those users 
# who have rated, for example, at least 50 films. We can also apply this threshold 
# to movies that have to be seen at least for a minimum number of users (e.g.: 50). 
# In this way, we have filtered a list of watched films from least-watched ones. We 
# can easily do that on our rating matrix:
movie_ratings = real_ratings[rowCounts(real_ratings)>50 , colCounts(real_ratings)>50]
movie_ratings

# We have reduced from 71567 users x 65133 movies to 3006 users x 2834 movies and 
# the sparsity ratio is lower (204983 / (3006 * 2834) = 0.024). We can also look for
# the most active users and most viewed movies:
minimum_movies =  quantile(rowCounts(movie_ratings),0.98)
minimum_movies

minimum_users  =  quantile(colCounts(movie_ratings),0.98)
minimum_users

# The heatmap of these top users and movies would be:
image(movie_ratings[rowCounts(movie_ratings)>minimum_movies, colCounts(movie_ratings)>minimum_users], main="HeatMap of top Users and Movies")

# And the rating matrix for these top users and movies would be:
new_movie_ratings <- movie_ratings[rowCounts(movie_ratings)>minimum_movies, colCounts(movie_ratings)>minimum_users]
new_movie_ratings

# As seen above, the top is made of 57 users and 55 movies. The sparsity ratio in 
# this case is 0.09 (287 / (57 * 55)).

#### Matrix Normalization
# An important operation for rating matrices is normalization. These operation is 
# important, as it allows us to eliminate the user bias, where users can be high 
# raters or low raters and have the adverse effect of distorting the rating 
# distribution. By normalizing the matrix, we remove the user rating bias by 
# subtracting the row mean from all ratings in the row. This is can be easily done 
# using normalize(). The *method* parameter allows to choose the type of 
# normalization, *center* or *Z-score*. The Z-score centering removes the row mean 
# from each row value and then divides by the standard deviation. By default, 
# *center* is applied.
normalized_ratings <- normalize(movie_ratings)
normalized_ratings

# The heatmap of the first 25 rows and columns would be:
image(normalized_ratings[1:25,1:25],axes=FALSE,main="HeatMap of 1st 25rows & 25columns (norm. matrix)")

# Using the Z-score centering
normalized_ratings <- normalize(movie_ratings, "Z-score")
normalized_ratings

# The heatmap of the first 25 rows and columns would be:
image(normalized_ratings[1:25,1:25],axes=FALSE,main="HeatMap of first 25  rows and 25 columns (normalized matrix)")

#### Matrix Binarization
# The rating matrix, realRatingMatrix, is sometimes useful to convert it into a 
# binary matrix. This is done by choosing a threshold rate. For this example we will 
# define a matrix that will consist of 1 if the rating is above the average rating 
# and 0 otherwise.
threshold <- mean(edx_cf$rating)
threshold

binary_matrix <- binarize(movie_ratings, minRating = threshold)
binary_matrix

# The heatmap of the first 25 rows and columns would be:
image(binary_matrix[1:25,1:25],axes=FALSE,main="HeatMap of first 25  rows and 25 columns (binarized matrix)")

### Collaborative Filtering (CF) Evaluation Schemas - Data Preparation
# The recommenderlab framework does the data split between train and test data on its
# own from input matrix of realRatingMatrix type. We will use the edx_cf dataset that
# created with a 10% of the edx one.

# To evaluate the different CF algorithms within the recommenderlab framework, we 
# need an evaluation schema, where we specify different parameters. These parameters
# are:
# - *method*: instructs how to split the input data in between train and test. We 
#   will be creating an evaluation schema for each of the available methods (split, 
#   cross-validation and bootstrap). 
# - *given*: indicates the number of randomly chosen items of the test users that 
#   are given to the recommender algorithm and the remaining items are withheld for 
#   evaluation. The value for the *given* parameter cannot exceed the number of rated
#   movies for the user who has rated the least movies. To find out this value:
min(rowCounts(real_ratings))

real_ratings
# We do have a problem with the value above, as it is zero and we want for CF 
# algorithms this *given* parameter to be greater than zero. Let's avoid those rows 
# (users) who have evaluated less than 10 or 20 movies:
real_ratings_small <- real_ratings[rowCounts(real_ratings)>10, colCounts(real_ratings)>10]
min(rowCounts(real_ratings_small))
real_ratings_small

real_ratings_small <- real_ratings[rowCounts(real_ratings)>20, colCounts(real_ratings)>20]
min(rowCounts(real_ratings_small))
real_ratings_small

# Let's go with the last value of 11. This means there minimum number of movies 
# rated by some of our users is 11, and we are constrained by that the number of 
# ratings to give to our algorithms. Let's pick 5 as the value for the *given* 
# parameter. This means that the minimum ratings of a user that we will keep as 
# "unknown" to the algorithms will be 6. It is the algorithm the one that, randomly, 
# chooses which ratings to give and which to keep/hide.

# Let's create now the different evaluation schemas:
# - The split method randomly assigns the proportion of objects specified by the 
#   *train* parameter to the training set and the rest is used for the test set.
set.seed(1, sample.kind="Rounding")
e_split <- evaluationScheme(real_ratings_small, method="split", train=0.8, given=15)

# - The cross-validation method creates a k-fold cross-validation scheme. The data is
#   randomly split into k parts (as indicated in the *k* parameter) and in each 
#   run k-1 parts are used for training and the remaining part is used for testing. 
#   After all k runs each part was used as the test set exactly once.
e_cross <- evaluationScheme(real_ratings_small, method="cross-validation", k=10, given=15)

# - The bootstrap method creates the training set by taking a bootstrap sample 
#   (sampling with replacement) of size train times number of users in the data set
#   (as indicated in the *train* parameter). All objects not in the training set are 
#   then used for testing.
e_bootstrap <- evaluationScheme(real_ratings_small, method="bootstrap", train=0.8, given=15)

### Collaborative Filtering (CF) Popular Model

#### Split
rec_popular <- Recommender(getData(e_split, "train"), "POPULAR")
predict_popular <- predict(rec_popular, getData(e_split, "known"), type="ratings")
class(predict_popular)

rmse_popular <- calcPredictionAccuracy(predict_popular, getData(e_split, "unknown"))
rmse_popular

# We can see these error figures for each individual user:
head(calcPredictionAccuracy(predict_popular, getData(e_split, "unknown"), byUser=TRUE))

#### Cross-Validation
rec_popular_k <- Recommender(getData(e_cross, "train"), "POPULAR")
predict_popular_k <- predict(rec_popular_k, getData(e_cross, "known"), type="ratings")
rmse_popular_k <- calcPredictionAccuracy(predict_popular_k, getData(e_cross, "unknown"))
rmse_popular_k

#### Bootstrap
rec_popular_b <- Recommender(getData(e_bootstrap, "train"), "POPULAR")
predict_popular_b <- predict(rec_popular_b, getData(e_bootstrap, "known"), type="ratings")
rmse_popular_b <- calcPredictionAccuracy(predict_popular_b, getData(e_bootstrap, "unknown"))
rmse_popular_b

# To carry results along, let's create a table where we can keep adding the outcomes 
# of our different models for final decision on best one. In the POPULAR model, the 
# best RMSE (0.9410653) is obtained for the split method.
errors <- bind_rows(errors,
                     tibble(algorithm = "POPULAR", method = "split", RMSE = 0.9410653))
errors %>% knitr::kable()

### User Based Collaborative Filtering (UBCF) Model

#### Split
rec_ubcf <- Recommender(getData(e_split, "train"), "UBCF")
predict_ubcf <- predict(rec_ubcf, getData(e_split, "known"), type="ratings")
rmse_ubcf <- calcPredictionAccuracy(predict_ubcf, getData(e_split, "unknown"))
rmse_ubcf

#### Cross-Validation
rec_ubcf_k <- Recommender(getData(e_cross, "train"), "UBCF")
predict_ubcf_k <- predict(rec_ubcf_k, getData(e_cross, "known"), type="ratings")
rmse_ubcf_k <- calcPredictionAccuracy(predict_ubcf_k, getData(e_cross, "unknown"))
rmse_ubcf_k

#### Bootstrap
rec_ubcf_b <- Recommender(getData(e_bootstrap, "train"), "UBCF")
predict_ubcf_b <- predict(rec_ubcf_b, getData(e_bootstrap, "known"), type="ratings")
rmse_ubcf_b <- calcPredictionAccuracy(predict_ubcf_b, getData(e_bootstrap, "unknown"))
rmse_ubcf_b

# The best result for UBCF is RMSE of 1.1891657 for cross-validation method
errors <- bind_rows(errors,
                     tibble(algorithm = "UBCF", method = "cross-val", RMSE = 1.1891657))
errors %>% knitr::kable()

### Item Based Collaborative Filtering (IBCF) Model

#### Split
rec_ibcf <- Recommender(getData(e_split, "train"), "IBCF")
predict_ibcf <- predict(rec_ibcf, getData(e_split, "known"), type="ratings")
rmse_ibcf <- calcPredictionAccuracy(predict_ibcf, getData(e_split, "unknown"))
rmse_ibcf

#### Cross-Validation
rec_ibcf_k <- Recommender(getData(e_cross, "train"), "IBCF")
predict_ibcf_k <- predict(rec_ibcf_k, getData(e_cross, "known"), type="ratings")
rmse_ibcf_k <- calcPredictionAccuracy(predict_ibcf_k, getData(e_cross, "unknown"))
rmse_ibcf_k

#### Bootstrap
rec_ibcf_b <- Recommender(getData(e_bootstrap, "train"), "IBCF")
predict_ibcf_b <- predict(rec_ibcf_b, getData(e_bootstrap, "known"), type="ratings")
rmse_ibcf_b <- calcPredictionAccuracy(predict_ibcf_b, getData(e_bootstrap, "unknown"))
rmse_ibcf_b

# The best result for IBCF is RMSE of 1.688562 for split method
errors <- bind_rows(errors,
                     tibble(algorithm = "IBCF", method = "split", RMSE = 1.344568))
errors %>% knitr::kable()

### Singular Value Decomposition (SVD) CF Model

#### Split
rec_svd <- Recommender(getData(e_split, "train"), "SVD")
predict_svd <- predict(rec_svd, getData(e_split, "known"), type="ratings")
rmse_svd <- calcPredictionAccuracy(predict_svd, getData(e_split, "unknown"))
rmse_svd

#### Cross-Validation
rec_svd_k <- Recommender(getData(e_cross, "train"), "SVD")
predict_svd_k <- predict(rec_svd_k, getData(e_cross, "known"), type="ratings")
rmse_svd_k <- calcPredictionAccuracy(predict_svd_k, getData(e_cross, "unknown"))
rmse_svd_k

#### Bootstrap
rec_svd_b <- Recommender(getData(e_bootstrap, "train"), "SVD")
predict_svd_b <- predict(rec_svd_b, getData(e_bootstrap, "known"), type="ratings")
rmse_svd_b <- calcPredictionAccuracy(predict_svd_b, getData(e_bootstrap, "unknown"))
rmse_svd_b

# The best result for SVD is RMSE of 1.0657260 for method bootstrap
errors <- bind_rows(errors,
                     tibble(algorithm = "SVD", method = "split", RMSE = 1.0433052))
errors %>% knitr::kable()

### Singular Value Decomposition Funk (SVDF) CF MODEL

#### Split
rec_svdf <- Recommender(getData(e_split, "train"), "SVDF")
predict_svdf <- predict(rec_svdf, getData(e_split, "known"), type="ratings")
rmse_svdf <- calcPredictionAccuracy(predict_svdf, getData(e_split, "unknown"))
rmse_svdf

#### Cross-Validation
rec_svdf_k <- Recommender(getData(e_cross, "train"), "SVDF")
predict_svdf_k <- predict(rec_svdf_k, getData(e_cross, "known"), type="ratings")
rmse_svdf_k <- calcPredictionAccuracy(predict_svdf_k, getData(e_cross, "unknown"))
rmse_svdf_k

#### Bootstrap
rec_svdf_b <- Recommender(getData(e_bootstrap, "train"), "SVDF")
predict_svdf_b <- predict(rec_svdf_b, getData(e_bootstrap, "known"), type="ratings")
rmse_svdf_b <- calcPredictionAccuracy(predict_svdf_b, getData(e_bootstrap, "unknown"))
rmse_svdf_b

# The best result for SVDF is RMSE of 0.9756173 for method split
errors <- bind_rows(errors,
                     tibble(algorithm = "SVDF", method = "split", RMSE = 0.9756173))
errors %>% knitr::kable()

### Recosystem Model
# The Recosystem is an R wrapper of the 'libmf' library 
# (http://www.csie.ntu.edu.tw/~cjlin/libmf/) for recommender system using matrix 
# factorization and typically used to approximate an incomplete matrix using the 
# product of two matrices in a latent space. The concept of latent space is related 
# to the latent factors defined in it and that will be tuned in some of the parameters
# used in the turne() method later. The main task of the recommender system is to 
# predict unknown entries in the rating matrix based on observed values.

# LIBMF itself is a parallelized library, meaning that users can take advantage of 
# multicore CPUs to speed up the computation. The recosystem is a wrapper of LIBMF, 
# hence the features of LIBMF are all included in recosystem. Also, unlike most other 
# R packages for statistical modeling which store the whole dataset and model object 
# in memory, LIBMF (and hence recosystem) is much hard-disk-based, for instance the 
# constructed model which contains information for prediction can be stored in the 
# hard disk, and prediction result can also be directly written into a file rather 
# than kept in memory. That is to say, recosystem will have a comparatively small 
# memory usage. This is noticed in the steps below where their input is read from a 
# file or their outcome written to a file.

# I could have used the LIBMF_realRatingMatrix model that comes within the 
# recommenderlab package, but preferred to used the recosystem package on its own to
# make things a bit different.

# We will use the edx_cf dataset from which we obtained the realRatingMatrix for the
# CF methods used so far. We will have to split it in between train and test sets, as 
# the recosystem model doesn't do the split on its own as previous CF models. But 
# first we filter it to select just the userId, movieId and rating columns, as that 
# is all we need
names(edx_cf)
edx_cf_clean <- select(edx_cf, userId, movieId, rating)
head(edx_cf_clean)

# Let's create train and test sets
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)
test_index <- createDataPartition(y = edx_cf_clean$rating, times = 1, p = 0.1, list = FALSE)
trainset <- edx_cf_clean[-test_index,]
testset <- edx_cf_clean[test_index,]

# Write trainset and testset tables on disk
write.table(trainset , file = "trainset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
write.table(testset, file = "testset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
train_set <- data_file("trainset.txt")
test_set <- data_file("testset.txt")
class(train_set)

# Create recommender
set.seed(2021, sample.kind="Rounding")
r = Reco()
class(r)

# We will use the tune() method to tune the model parameters. This method uses 
# cross validation to tune the model parameters.These parameters are:
# - dim: the number of latent factors. I have used the 3 values given as example on 
#   the tune() method page (https://www.rdocumentation.org/packages/recosystem/versions/0.3/topics/tune)
# - lrate: the learning rate, which can be thought of as the step size in gradient 
#   descent. As for previous one, I've used the 3 values given as example on the tune()
#   method page on the web.
# - nthread: integer, the number of threads for parallel computing. Default is 1 and
#   I have kept it as that.
# - niter: integer, the number of iterations. Default is 20. I have used 20 for this 
#   case as well as the for the final use of recosystem on the validation set.

# The output of this method is a list with two components:
# - min: parameter values with minimum cross validation RMSE. This is a list that 
#   can be passed to the opts argument in $train() and we will do that in the call 
#   to the train() method.
# - res: a data frame giving the supplied candidate values of tuning parameters, and
#   one column showing the RMSE associated with each combination.
opts = r$tune(train_set, opts = list(dim = c(10, 20, 30), lrate = c(0.05, 0.1, 0.2),
                                          nthread = 1, niter = 10))

# We can have a look at the parameters providing the minimum RMSE value with:
opts$min

# Let's train our model. The train() method trains a recommender model. It will 
# read a training data file and create a model file at the specified locations. The
# model file contains necessary information for prediction.
r$train(train_set, opts = c(opts$min, nthread = 1, niter = 20))

# Let's generate the predictions and write them to a file on disk.
pred_file <- tempfile()
r$predict(test_set, out_file(pred_file)) 

# We can read the first 10 predicted values from the file
print(scan(pred_file, n = 10))

# Let's read the predictions from file.
predicted_ratings_reco <- scan(pred_file)

class(predicted_ratings_reco)
str(predicted_ratings_reco)
# Let's calculate the RMSE
rmse_reco <- RMSE(predicted_ratings_reco,testset$rating)
rmse_reco

errors <- bind_rows(errors,
                    tibble(algorithm = "Recosystem", method = "    ", RMSE = 0.9258568))
errors %>% knitr::kable()

### Regularized bias effect of movie and user
# To be able to compare the results of the CF models with the model of regularized 
# biases of predictors, they should all be using the same data. The CF models did 
# make use only of movie, user and rating columns from the edx_cf dataset, which is 
# a subset of the edx one used on the bias model. Let's apply the bias model on the 
# edx_cf dataset and only taking into account information on movie, user and rating.

# Let's split the edx_cf into train and test portions.
set.seed(2021, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
cf_test <- createDataPartition(y = edx_cf$rating, times = 1, p = 0.1, list = FALSE)
edx_cf_train <- edx_cf[-cf_test,]
temp <- edx_cf[cf_test,]
edx_cf_test <- temp %>% 
  semi_join(edx_cf_train, by = "movieId") %>%
  semi_join(edx_cf_train, by = "userId")
removed <- anti_join(temp, edx_cf_test)
nrow(removed)
edx <- rbind(edx_cf_train, removed)
nrow(edx_cf_train)
nrow(edx_cf_test)

# Let's calculate the mean for our edx_cf set.
mu <- mean(edx_cf_train$rating)
mu

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  
  b_i <- edx_cf_train %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx_cf_train %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    edx_cf_train %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, edx_cf_train$rating))
})

# Plotting lambdas versus rmses:
qplot(lambdas, rmses)

# The minimum value of the RMSE obtained using the train set is:
min(rmses)

# The lambda that minimizes the RMSE is:
lambda <- lambdas[which.min(rmses)]
lambda

# Compute regularized estimates of b_i using lambda
movie_avgs_reg <- edx_cf_train %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())

# Compute regularized estimates of b_u using lambda
user_avgs_reg <- edx_cf_train %>% 
  left_join(movie_avgs_reg, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+lambda), n_u = n())

# Predict ratings for our test set:
predicted_ratings_cf <- edx_cf_test %>% 
  left_join(movie_avgs_reg, by='movieId') %>%
  left_join(user_avgs_reg, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>% 
  .$pred

# Calculate RMSE against validation set with generated prediction:
rmse_cf <- RMSE(edx_cf_test$rating,predicted_ratings_cf)
rmse_cf

# As we can see, the RMSE is higher than when using full edx set and all the 
# predictors, but still better than any of our CF models.
errors <- bind_rows(errors,
                    tibble(algorithm = "REG BIAS CF", method = "    ", RMSE = 0.905792))
errors %>% knitr::kable()

## Results
# As we can see, the best RMSE result of all the CF models is achieved by the 
# Recosystem one. We have already proved the model that takes into account all the 
# regularized bias effect of all the predictors is capable of taking the RMSE below
# the RMSE threshold of 0.86490 and even that type of model operating on the same 
# dataset (edx_cf) as the CF models gives an RMSE (0.905792) better than the CF 
# models. So, let's see if the best of the CF models, the recosystem, is capable 
# of outperforming the bias model result on the full edx and validation datasets.
nrow(edx)
nrow(validation)

# Let's get the matrix for both edx and validation datasets
train_for_mf <- edx %>% 
  select(userId, movieId, rating)
train_for_mf <- as.matrix(train_for_mf)
val_for_mf <- validation %>% 
  select(userId, movieId, rating)
val_for_mf <- as.matrix(val_for_mf)

# Write filtered train and validation sets to disk
write.table(train_for_mf , file = "trainset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)
write.table(val_for_mf, file = "valset.txt" , sep = " " , row.names = FALSE, col.names = FALSE)

# Create data sets from written files in correct format for recosystem
train_set <- data_file("trainset.txt")
val_set <- data_file("valset.txt")
class(train_set)

# Use the recosystem library to perform the matrix factorization
# First, let's build the recommender
set.seed(2021, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
r <-Reco()

# Let's tune the training set
opts <- r$tune(train_set, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                      costp_l1 = 0, costq_l1 = 0,
                                      nthread = 1, niter = 10))


# Let's train the model
r$train(train_set, opts = c(opts$min, nthread = 1, niter = 20))

# Make predictions on the validation set 
pred_file <- tempfile()
r$predict(val_set, out_file(pred_file)) 

# Let's read predictions file
predicted_mf <- scan(pred_file)

# Let's see what is our predictions object
str(predicted_mf)

# Let's check our validation object
str(validation$rating)

# As seen above, both are of the same type and length. Let's calculate the RMSE:
rmse_mf <- RMSE(predicted_mf,validation$rating)
rmse_mf

## Conclusions
# Using the recosystem collaborative filtering model, that uses matrix factorization,
# the final RMSE achieved on the validation set has been 0.7829094, which goes below 
# the lowest threshold set in the project (0.86490) and improves quite a bit the 
# outcome (0.8623166) of the model of regularized bias for all predictors created in 
# the first model of this document.

# I was expecting better results from the UBCF and IBCF models, in particular the 
# UBCF, as all the readings I found about CF models was putting it as the best one 
# producing results, but their results were worse than the POPULAR, SVD and SVDF 
# models.

# Computing resources (MacBook Pro with 8GB of RAM) have been a bottle neck in this 
# project when working with CF models, like UBCF and IBCF, using the edx dataset.
# Generating evaluation schemas caused the R session to hung and the laptop to run 
# out of memory. That was the main reason to generate the edx_cf dataset (900K rows)
# as a 10% of the edx one (9M records), although I had already verified all CF models
# run OK with the 1M Movielens dataset (see Appendix A for its link). Even with that, 
# R session did hung several times when training some of the CF models and required
# to rerun the step again after restarting R.

# Appendix A - Bibliography
# - [MovieLens](https://grouplens.org/datasets/movielens/)
# - [MovieLens 10M Dataset](https://files.grouplens.org/datasets/movielens/ml-10m.zip)
# - [MovieLens 1M Dataset](https://files.grouplens.org/datasets/movielens/ml-1m.zip)
# - [recommenderlab: A Framework for Developing and Testing Recommendation Algorithms](https://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf)
# - [Recommender: Create a Recommender Model](https://rdrr.io/cran/recommenderlab/man/Recommender.html)
# - [Recommender System — Matrix Factorization](https://towardsdatascience.com/recommendation-system-matrix-factorization-d61978660b4b)
# - [RecommenderLab Tutorial](https://github.com/BrandonHoeft/Recommender-System-R-Tutorial/blob/master/RecommenderLab_Tutorial.md)
# - [recommenderlab: Lab for Developing and Testing Recommender Algorithms](https://cran.r-project.org/web/packages/recommenderlab/index.html)
# - [Package ‘recommenderlab’](https://cran.r-project.org/web/packages/recommenderlab/recommenderlab.pdf)
# - [Sistemas de recomendación en R (Recommendation systems in R)](http://rstudio-pubs-static.s3.amazonaws.com/432515_2ec5e23a0c5c4730bf30c12d26537818.html)
# - [Movie Recommendation System](https://rpubs.com/vamshigvk/whatsthenextmovie)
# - [Predicting Ratings with Matrix Factorization Methods](https://medium.com/beek-tech/predicting-ratings-with-matrix-factorization-methods-cf6c68da775)
# - [Recosystem - Recommender System using Matrix Factorization](https://www.rdocumentation.org/packages/recosystem/versions/0.3)
