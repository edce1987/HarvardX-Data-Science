#Introduction
#You will use the following code to generate your datasets. 
#Develop your algorithm using the edx set. For a final test of your final 
#algorithm, predict movie ratings in the validation set (the final hold-out 
#test set) as if they were unknown. RMSE will be used to evaluate how close 
#your predictions are to the true values in the validation set (the final 
#hold-out test set).
#
#Important: The validation data (the final hold-out test set) should NOT be 
#used for training, developing, or selecting your algorithm and it should ONLY 
#be used for evaluating the RMSE of your final algorithm. You should split the 
#edx data into separate training and test sets to design and test your algorithm.
#
#Also remember that by accessing this site, you are agreeing to the terms of 
#the edX Honor Code. This means you are expected to submit your own work and 
#can be removed from the course for substituting another student's work as your 
#own.

library(tidyverse)
library(caret)
library(dslabs)

#Q1
dim(edx)

#Q2
edx %>% select(rating) %>% group_by(rating) %>% summarise(n=n()) 

#or
edx %>% filter(rating == 0) %>% tally()

#Q3
edx %>% select(movieId) %>% group_by(movieId) %>% distinct(movieId) %>% nrow()

#or
n_distinct(edx$movieId)

#Q4
edx %>% select(userId) %>% distinct(userId) %>% nrow()

#or
n_distinct(edx$userId)

#Q5

drama <- str_detect(edx$genres, "Drama")

comedy <- str_detect(edx$genres, "Comedy")

thriller <- str_detect(edx$genres, "Thriller")

romance <- str_detect(edx$genres, "Romance")

length(romance[romance == TRUE])

#or
# str_detect
genres = c("Drama", "Comedy", "Thriller", "Romance")
sapply(genres, function(g) {
  sum(str_detect(edx$genres, g))
})

# separate_rows, much slower!
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#Q6
edx %>% group_by(title) %>% summarise(n=n()) %>% arrange(desc(n))

#or
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#Q7
edx %>% group_by(rating) %>% summarise(n=n()) %>% arrange(desc(n))

#or
edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>%
  arrange(desc(count))

#Q8
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()

####

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