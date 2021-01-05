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

#Split edx data into train and test set
set.seed(1, sample.kind = "Rounding")

#Create index to split data (80% train & 20% test)
testIndex <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
trainSet <- edx[-testIndex,]
testSet <- edx[testIndex,]

#Make sure UserId and MovieId are existing in train and test set
testSet <- testSet %>% 
  semi_join(trainSet, by = "movieId") %>%
  semi_join(trainSet, by = "userId")

#Define function to compute Root Mean Squared Error (RMSE) based on true outcome and prediction
RMSE <- function(true, predicted){
  sqrt(mean((true - predicted)^2))
}

#Prepare sample of train and test set

trainShort <- sample_n(trainSet, 1000)
testShort <- sample_n(testSet, 1000)

testShort <- testShort %>% 
  semi_join(trainShort, by = "movieId") %>%
  semi_join(trainShort, by = "userId")

#KNN
modelLookup("knn")
fitKnn <- train(rating ~ ., method = "knn", data = trainShort, tuneGrid = data.frame(k=seq(1,10,1)))
fitKnn
ggplot(fitKnn)

predKnn <- predict(fitKnn, testShort)
predKnn

rmseKnn <- RMSE(testShort$rating, predKnn)
rmseKnn

#Logistic
modelLookup("glm")
fitGlm <- train(rating ~ ., method = "glm", data = trainShort)
fitGlm


predGlm <- predict(fitGlm, testShort)
predGlm

rmseGlm <- RMSE(testShort$rating, predGlm)
rmseGlm

#Random Forest
modelLookup("rf")
fitRf <- train(rating ~ ., method = "rf", data = trainShort, tuneGrid = data.frame(mtry=seq(1,10,1)))
fitRf
ggplot(fitRf)

predRf <- predict(fitRf, testShort)

rmseRf <- RMSE(testShort$rating, predRf)
rmseRf


#LM
modelLookup()
fitLm <- train(rating ~ ., method = "lm", data = trainShort)
fitLm

predLm <- predict(fitLm, testShort)

rmseLm <- RMSE(testShort$rating, predLm)
rmseLm

#Loess
modelLookup()
fitGamLoess <- train(rating ~ ., method = "gamLoess", data = trainShort)
fitGamLoess

predGamLoess <- predict(fitGamLoess, testShort)

rmseGamLoess <- RMSE(testShort$rating, predGamLoess)
rmseGamLoess

######


