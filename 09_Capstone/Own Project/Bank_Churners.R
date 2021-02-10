## Introduction
# The following context / background information is taken from the below source link. I have summarized it such that it conveys the key information.
# Background: A manager at a bank is concerned with more and more customers leaving their credit card services (i.e. churning). 
# The bank manager would like to predict which customer is going to churn next based on the underlying data.
# His goal is to identify likely-to-churn customers such that the bank can proactively contact the customer to provide a customized service or a special offer to prevent him from churning.
# The dataset is originally from a website with the URL as https://leaps.analyttica.com/home.
# It contains 10,127 customers with information about their age, salary, marital_status, credit card limit, credit card category, etc. In total there are 23 variables.
# Limitations: Since the dataset has approximately 16.07% of customers who churned, it might be difficult to train a model that reliably predicts churning customers.
# Source: https://www.kaggle.com/sakshigoyal7/credit-card-customers?select=BankChurners.csv

# Install required packages.
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(DataExplorer)) install.packages("DataExplorer", repos = "http://cran.us.r-project.org")

# Load required libraries.
library(tidyverse)
library(caret)
library(Rborist)
library(rpart)
library(DataExplorer)
library(data.table)

# To my knowledge, a direct download from Kaggle is not possible.
# There is however a workaround using your Kaggle login name and password but we chose another way here.
# For the sake of simplicity, let us follow these steps:
# 1. Download the file using below link. It should automatically be named as "archive.zip".
# 2. Save the downloaded "archive.zip" into your working directory. This is important, otherwise it will not work.
# Source: https://www.kaggle.com/sakshigoyal7/credit-card-customers/download

# Use the following code to get or set your wd accordingly.
getwd()
setwd("/Users/edce/projects/HarvardX-Data-Science/09_Capstone/Own Project")

## Data Input

# Unzip, extract and read the data from archive.zip.
# The archive contains a csv-file with a header and strings. Hence, header is true and strings are converted to factors directly.
# I noticed when using the following code with read.csv(...) on a MacBook, the data was not imported correctly (only 9857 rows). 
# The correct import of the dataset should have a header and 10.127 rows.
data <- read.csv(unz("archive.zip", filename = "BankChurners.csv"), header = TRUE, stringsAsFactors = TRUE)
# If you encounter above mentioned error, please use the following code that uses fread(...)
data <- fread(cmd = 'unzip -cq archive.zip', header = TRUE, stringsAsFactors = TRUE)

# Inspect if conversion was done correctly.
str(data)

## Data Exploration

# To quickly explore the dataset, we use the package "DataExplorer" which is a very useful package to quickly generate a report that gives you a detailed insight into you dataset.
DataExplorer::create_report(data)

# From the report we receive basic statistics about our dataset, i.e. rows, columns etc.
# We also see that there are no NAs in the columns, so we don't have to fill them.
# We see that our target variable "Attrition_Flag" is discrete, and that we have 26% discrete columns and 74% numeric columns in our dataset.
# We see the distributions of the underlying data, some are approximately normally distributed, others are not. Hence, we should try not use models that strictly assume normality of the data.
# We also see a correlation analysis of the variables and principal component analysis which will become handy later on.

## Data Cleaning

# From the above Kaggle link, from the author we receive the information that the variables "Naive_Bayes_Classifier...._1" and "Naive_Bayes_Classifier...._2" should be removed from the dataset.  
data_clean <- data %>% select(-Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1 & -Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2)
str(data_clean)

## Model: Preparation

# Create train set (80%) and test set (20%). 
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(data_clean$Attrition_Flag, times = 1, p = 0.2, list = FALSE)
train_set <- data_clean[-test_index,]
test_set <- data_clean[test_index,]

## Model: Selection

# We want to predict the "Attrition_Flag" which is a discrete variable.
# Hence, we need a model that it appropriate for classification purposes.
# We inspect the list of models from the "caret" package that are suitable for classification.
mod <- modelLookup()
mod <- mod %>% filter(forClass == TRUE)
View(mod)

# We select a set of appropriate models and store them into the "models" variable.
models <- c("adaboost", "bayesglm", "knn", "naive_bayes", "nnet", "Rborist", "rf", "rpart", "svmLinear", "svmPoly")

## Model: Design, Training & Prediction

#  We use 10-fold cross validation to account for overfitting and selection bias.
set.seed(1, sample.kind="Rounding")
control <- trainControl(method = "cv", number = 10, p = .8)

# The selected models are fitted and predictions are made as part of an sapply statement.
# Attention: This takes several minutes or even an hour depending on your hardware.
set.seed(1, sample.kind="Rounding")
results <- sapply(models, function(model) {
  print(model)
  fit <- train(Attrition_Flag ~ ., method = model, trControl = control, data = train_set)
  prediction <- predict(fit, test_set)
  data.frame(client = test_set$CLIENTNUM, model = prediction)
})
results <- as.data.frame(results)

# Inspect results.
View(results)

## Model: Evaluation
# We evaluate the predictions and compute a confusion matrix for each model.
# The confusion matrix is a table that describes the performance of a classification model.
# From it you can derive several parameters, i.e. accuracy, sensitivity, specificity and prevalence among others.
# We will use multiple metrics to evaluate our models.
# We will use the accuracy and the F-measure to determine the performance of our models. 
# Theory: The accuracy is the number of correct predictions in relation to total predictions made.
# Furthermore, we will use the F1 score or F measure. 
# Theory: It is a number between 0 and 1 and is the harmonic mean of precision and recall. It balances precision and recall.
# Theory: Precision (or Pos. Pred Value) is the share of correct positive predictions (True Positives) in relation to the total positive predictions (True Positives + False Positives).
# Theory: Recall (or Specificity) is the share of correct positive predictions (True Positives) in relation to the total positives (True Positives + False Negatives). 

# We compute the accuracy for the used models.
accuracies <- sapply(results, function(x) {
  confusionMatrix(data=x, reference=test_set$Attrition_Flag)$overall["Accuracy"]
})
accuracies

# The "AdaBoost" model has the highest accuracy.
accuracies[which.max(accuracies)]

# Let us now look at the F_measures for the used models.
f_measures <- sapply(results, function(x) {
  F_meas(data=x, reference=test_set$Attrition_Flag)
})
f_measures

# The "AdaBoost" model has the highest F-measure.
f_measures[which.max(f_measures)]

# The "AdaBoost" model looks very promising, let us see if we can optimize it by using a larger tune grid.
fitAdaboost <- train(Attrition_Flag ~ ., method = "adaboost", tuneGrid = data.frame(nIter = seq(1,1000,100), method = "Adaboost.M1"), trControl = control, data = test_set)
fitAdaboost
predAdaboost <- predict(fitAdaboost, test_set)
confusionMatrix(data=predAdaboost, reference=test_set$Attrition_Flag)

# Inspect the final model parameters.
ggplot(fitAdaboost)
fitAdaboost$bestTune
fitAdaboost$finalModel

# We can also check the variable importance to see which variables have the largest explanatory power.
# From that, we see that the variables are very important to identify potentially churning customers.
varImp(fitAdaboost)

# Finally, we can also check whether an ensemble of the selected models will give us better results than the AdaBoost model alone.
# To build an ensemble, we observe the predictions by each model and determine a majority vote.
# Hence, the prediction of the ensemble model is the majority vote of the underlying 10 models.
votes <- rowMeans(results == "Attrited Customer")

# Prediction with ensemble
predEnsemble <- as.factor(ifelse(votes > 0.5, "Attrited Customer", "Existing Customer"))

# Determination of accuracy and F-measure
confusionMatrix(data=predEnsemble, reference=test_set$Attrition_Flag)$overall["Accuracy"]
F_meas(data=predEnsemble, reference=test_set$Attrition_Flag)

# Evaluation of Ensemble
# Although the ensemble model performs also quite well, it is not better than the "AdaBoost" model alone.
# Hence, we will select the "AdaBoost" model as our final model.

## Summary
# From our previous results, we see that the "Adaboost" model has the highest accuracy and F-measure.
# The Adaboost model correctly predicted 96% of the Attrited Customers.
# For the use case at hand, the bank manager could predict which customer is likely to churn.
# With this knowledge, he can apply countermeasures by e.g. providing a customized service to the client or offering special offers to him.

## Limitations
# Since the underlying dataset has has a prevalence of 16% for the attributed customers, it may not be reliable enough to have a robust prediction of churning customers.
# However, the model performs very well with the data at hand and can be useful as a basis for further development.
