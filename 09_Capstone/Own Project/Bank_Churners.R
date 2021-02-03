#Context
#A manager at the bank is disturbed with more and more customers leaving their credit card services. They would really appreciate if one could predict for them who is gonna get churned so they can proactively go to the customer to provide them better services and turn customers' decisions in the opposite direction
#I got this dataset from a website with the URL as https://leaps.analyttica.com/home. I have been using this for a while to get datasets and accordingly work on them to produce fruitful results. The site explains how to solve a particular business problem.
#Now, this dataset consists of 10,000 customers mentioning their age, salary, marital_status, credit card limit, credit card category, etc. There are nearly 18 features.
#We have only 16.07% of customers who have churned. Thus, it's a bit difficult to train our model to predict churning customers.
#Source: https://www.kaggle.com/sakshigoyal7/credit-card-customers?select=BankChurners.csv

#Load libraries
library(tidyverse)
library(caret)
library(gam)
library(rpart)
library(matrixStats)
library(randomForest)
library(DataExplorer)

#A direct download from Kaggle is not possible, there is a workaround using 
#your login name and password which we will not do here.
#Download file and save it to YOUR working directory and extract data
#Source: https://www.kaggle.com/unsdsn/world-happiness/download

#The above file (archive.zip) has to be in your working directory in order for the following code to work.
#Use the following code to set your wd accordingly
getwd()
setwd("C:/Users/edin.ceman/Documents/GitHub/HarvardX-Data-Science/09_Capstone/Own Project")

#Unzip and extract data from archive.zip
dataset <- read.csv(unz("archive.zip", filename = "BankChurners.csv"), header = TRUE) 

#Remove redundant or irrelevant data
DataExplorer::create_report(dataset)

#Create function to calculate the root mean squared error (RMSE)
RMSE <- function(true, predicted){
  sqrt(mean((true - predicted)^2, na.rm = TRUE))
}

#Create train and test set
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(data_clean$Score, times = 1, p = 0.1, list = FALSE)
train_set <- data_clean[-test_index,]
test_set <- data_clean[test_index,]

#Choose models that are applicable to regression for ensemble creation
mod <- modelLookup()
mod <- mod %>% filter(forReg == "TRUE")
models <- c("lm", "glm", "knn", "rf", "gamLoess", "rpart", "Rborist")
#No Tune Grid

#Fit models, predict and calculate RMSE with usage of 10-fold cross validation
set.seed(1, sample.kind="Rounding")
control <- trainControl(method = "cv", number = 10, p = .9)
results <- sapply(models, function(model) {
  print(model)
  fit <- train(Score ~ ., method = model, trControl = control, data = train_set)
  pred <- predict(fit, test_set)
  data.frame(prediction = pred)
})
results

df <- as.data.frame(results)
df

rmses <- sapply(results, function(pred) {
  RMSE(pred, test_set$Score)
})
rmses

pred_ensemble <- rowMeans(df)
pred_ensemble

RMSE(test_set$Score, pred_ensemble)

#Looks like the random forest and der Rborist models have the best prediction
#power, even better than the ensemble model. Hence, we will focus on those two
#and find the best parameters using the tune grid.
set.seed(1, sample.kind="Rounding")
fitRf <- train(Score ~ ., method = "rf", trControl = control, tuneGrid = data.frame(mtry=seq(1,10,0.1)), data = train_set)
ggplot(fitRf)
fitRf$bestTune
fitRf$finalModel
predRf <- predict(fitRf, test_set)
RMSE(predRf, test_set$Score)

fitRandomForest <- randomForest(Score ~ ., train_set)
predRandomForest <- predict(fitRandomForest, test_set)
RMSE(predRandomForest, test_set$Score)

imp <- as.data.frame(importance(fitRandomForest)) %>% arrange(desc(IncNodePurity))
imp
