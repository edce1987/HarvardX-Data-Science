#Context
#The World Happiness Report is a landmark survey of the state of global 
#happiness. The first report was published in 2012, the second in 2013, the 
#third in 2015, and the fourth in the 2016 Update. The World Happiness 2017, 
#which ranks 155 countries by their happiness levels, was released at the United 
#Nations at an event celebrating International Day of Happiness on March 20th. 
#The report continues to gain global recognition as governments, organizations 
#and civil society increasingly use happiness indicators to inform their 
#policy-making decisions. Leading experts across fields – economics, psychology, 
#survey analysis, national statistics, health, public policy and more – describe 
#how measurements of well-being can be used effectively to assess the progress 
#of nations. The reports review the state of happiness in the world today and 
#show how the new science of happiness explains personal and national variations 
#in happiness.
#Source: https://www.kaggle.com/unsdsn/world-happiness

#Load libraries
library(tidyverse)
library(caret)
library(gam)
library(rpart)
library(matrixStats)
library(randomForest)

#A direct download from Kaggle is not possible, there is a workaround using 
#your login name and password which we will not do here.
#Download file and save it to YOUR working directory and extract data
#Source: https://www.kaggle.com/unsdsn/world-happiness/download

#The above file (archive.zip) has to be in your working directory in order for the following code to work.
#Use the following code to set your wd accordingly
#getwd()
#setwd()

#Unzip and extract data from archive.zip
dataset <- read.csv(unz("archive.zip", filename = "2019.csv"), header = TRUE) 

#Remove redundant or irrelevant data -> Overall rank is almost perfectly 
#negative correlated with the dependent variable Score which makes sense since
#the rank is decided based on the score. Country or region is unique for each 
#row in the data set, hence it does not provide any explanatory power.
cor(dataset$Overall.rank, dataset$Score)
identical(n_distinct(dataset$Country.or.region), nrow(dataset))

data_clean <- dataset %>% select(-Overall.rank, -Country.or.region)
str(data_clean)

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
