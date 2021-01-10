#The submission for the MovieLens project will be three files: a report in the 
#form of an Rmd file, a report in the form of a PDF document knit from your Rmd 
#file, and an R script that generates your predicted movie ratings and calculates
#RMSE. Your grade for the project will be based on two factors:
#  
#Your report and script (75%)
#The RMSE returned by testing your algorithm on the validation set (the final 
#hold-out test set) (25%)
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
#Peer grades are due at course close, on January 15, 2021, at 23:59 UTC.
#
#Peer Feedback
#You are strongly encouraged to give your peers thoughtful, specific written 
#feedback in addition to the numerical grades in the rubic. Think about the 
#type of feedback that would help you improve your work and offer that type of 
#feedback to your fellow learners.

#Load libraries
library(tidyverse)
library(caret)
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
lambdas <- seq(4.95, 5.15, 0.001)
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
    .$pred 
     return(RMSE(predicted_ratings, validation$rating)) #Calculate RMSE.
})

#Print final RMSE
finalRmse
