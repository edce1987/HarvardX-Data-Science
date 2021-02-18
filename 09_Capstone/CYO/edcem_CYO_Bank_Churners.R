## 1. Introduction
# The following context / background information is taken from the below source 
# link. I have summarized it such that it conveys the key information.

# Background: A manager at a bank is concerned with more and more customers 
# leaving their credit card services (i.e. churning). The bank manager would 
# like to predict which customer is going to churn next based on the underlying 
# data. His goal is to identify likely-to-churn customers such that the bank 
# can proactively contact the customer to provide a customized service or a 
# special offer to prevent him from churning. The dataset is originally from a 
# website with the URL as https://leaps.analyttica.com/home. It contains 10,127 
# customers with information about their age, salary, marital_status, credit 
# card limit, credit card category, etc. In total there are 23 variables.

# Source:
# https://www.kaggle.com/sakshigoyal7/credit-card-customers/

# Install & load required packages.
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(DataExplorer)) install.packages("DataExplorer", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(curl)) install.packages("curl", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org", dependencies = TRUE)

## 2.1 Data Initiation (Main solution)
# There are different ways to access the necessary data set.
# I figured out that the most convenient way is by using the package "curl" and
# to download the *.csv file directly from my public GitHub account. Should you 
# experience any issues with that please consider the alternative ways shown below.
data <- read.csv(curl("https://raw.githubusercontent.com/edce1987/edx_edcem_CYO/main/BankChurners.csv"), header = TRUE, stringsAsFactors =  TRUE)

# From the above Kaggle link, from the author we receive the information that 
# the variables "Naive_Bayes_Classifier...._1",  "Naive_Bayes_Classifier...._2" 
# should be removed from the dataset.  
data_clean <- data %>% select(-Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1 & -Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2)

# Inspect data.
# Please note that all strings are directly converted to factors.
str(data_clean)

#### You can skip this part and continue with "Data Exploration" directly. ####

## Data Initiation (Alternative solution)

# To my knowledge, a direct download from Kaggle is not easily possible with R.
# There seems to be a workaround using your Kaggle login name and password but 
# we will chose a more straightforward way here. For the sake of simplicity, 
# let us follow these steps:
# 1. Download the file using below Source link. It might be necessary to login
# into your Kaggle account first. The downloaded file is named "archive.zip".
# 2. Save the downloaded "archive.zip" into your working directory. This step 
# is important, otherwise it will not work.

# Use the following code to get or set your working directory accordingly.
#getwd("")
#setwd("")

# Source: https://www.kaggle.com/sakshigoyal7/credit-card-customers/download

# Unzip, extract and read the data from archive.zip.
# The archive contains a csv-file with a header and strings. Hence, header is 
# true and strings are converted to factors directly. I noticed when using the 
# following code with read.csv(...) on a MacBook, the data was not imported 
# correctly (only 9,857 rows). The correct import of the dataset should have a 
# header and 10.127 rows.
#data <- read.csv(unz("archive.zip", filename = "BankChurners.csv"), header = TRUE, stringsAsFactors = TRUE)
# If you experience above mentioned error, please use the following code that 
# uses fread(...) instead.
#data <- fread(cmd = 'unzip -cq archive.zip', header = TRUE, stringsAsFactors = TRUE)


## 2.2 Data Exploration
# To give you a better insight into the data, I have listed the included
# variables and a description of the name, type and meaning. All the information
# can be found in the above source link.

# The data set has the following variables:
# CLIENTNUM (int) -> Unique customer ID.
# Attrition_Flag (Factor w/ 2 levels "Attrited Customer", "Existing Customer") -> Flag if customer churned.
# Customer_Age (int) -> Age of the customer.
# Gender (Factor w/ 2 levels "F","M") -> Sex female or male.
# Dependent_count (int) -> Number of dependents.
# Education_Level (Factor w/ 7 levels "College","Doctorate",) -> Education level.
# Marital_Status (Factor w/ 4 levels "Divorced","Married",)
# Income_Category (Factor w/ 6 levels ") -> discrete income range, e.g. $60K - $80K.
# Card_Category (Factor w/ 4 levels "Blue","Gold",) -> tier of card product.
# Months_on_book (int) -> period of relationship with bank.
# Total_Relationship_Count (int) -> total no. of products held by the customer.
# Months_Inactive_12_mon (int) -> no. of months inactive in the last 12 months.
# Contacts_Count_12_mon (int) -> no. of contacts in the last 12 months.
# Credit_Limit (num) -> credit limit on the credit card.
# Total_Revolving_Bal (int) -> total revolving balance on the credit card.
# Avg_Open_To_Buy (num) -> open to buy credit line (average of last 12 months).
# Total_Amt_Chng_Q4_Q1 (num) -> change in transaction amount (Q4 over Q1).
# Total_Trans_Amt (int) -> total transaction amount (last 12 months).
# Total_Trans_Ct (int) -> total transaction count (last 12 months).
# Total_Ct_Chng_Q4_Q1 (num) -> change in transaction count (Q4 over Q1).
# Avg_Utilization_Ratio (num) -> average card utilization ratio.

# To quickly explore the dataset in more detail, we could simply use the package 
# "DataExplorer" which is a very powerful package to quickly generate a report 
# that gives you a detailed insight into your dataset.
# DataExplorer::create_report(data_clean)

# From the generated report (html) we receive a broad range of information. 
# Among others, we get basic statistics of our data set, i.e. number of rows, 
# columns etc. We also get information on the data structure, i.e. we see the 
# available variables in the data set. We also see that there are no missing 
# data points or NAs in the columns, so we don't have to fill them later on. 
# We see that our target variable "Attrition_Flag" (the indicator whether a
# customer has churned) is discrete / a factor. We see that we have 26% 
# discrete columns and 74% numeric columns in our data set. We see the 
# distributions and QQ-Plots of the underlying data - some are approximately 
# normally distributed, while others are not. Hence, we should avoid models 
# that strictly assume normality of the data. We also see a correlation analysis 
# of the variables and principal component analysis.

# However, since it would be "strange" to have a report generated in a report, 
# let us perform some of the data exploration steps manually.

# Plot basic data insights.
plot_intro(data_clean, title = "Data Exploration: Basics")

# Plot for missing values.
plot_missing(data_clean, title = "Data Exploration: Missing Values")

# Plot histogram.
plot_histogram(data_clean, title = "Data Exploration - Histogram")

# Plot bar diagram
plot_bar(data_clean, title = "Data Exploration - Exemplary Variable Characteristics")

## 2.3 Data Preparation
#Inspect dataset
View(data_clean)

# It makes sense to remove the variable *CLIENTNUM* which is a unique ID for each customer. Since this is a randomly assigned and unique ID, it is plausible to expect that is has no explanatory power. Since no e.g. *joins* are performed later on, we also do not need it for mapping purposes. Also, we want to avoid causing a potential interference for the models. Hence, we remove the *CLIENTNUM* from the data set.
data_prepared <- data_clean %>% select(-CLIENTNUM)

# Remove obsolete sets.
rm(data, data_clean)

# Create train set (80%) and test set (20%). 
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use set.seed(1)
test_index <- createDataPartition(data_prepared$Attrition_Flag, times = 1, p = 0.2, list = FALSE)
train_set <- data_prepared[-test_index,]
test_set <- data_prepared[test_index,]


## 2.4 Model Design
# We want to predict potentially churning customers which is indicated by the 
# variable "Attrition_Flag". It is a discrete or factor variable.
# Hence, we need a model that it appropriate for classification purposes.
# We inspect the list of models from the "caret" package that are suitable for 
# classification. We select a variety of models to find the best one.
mod <- modelLookup()
mod <- mod %>% filter(forClass == TRUE)
View(mod)

# We select a set of 10 suitable models and store them into the "models" variable.
models <- c("adaboost", "bayesglm", "knn", "naive_bayes", "Rborist", "rf", "rpart", "svmLinear", "svmPoly", "svmRadial")

# We use 10-fold cross validation to account for overfitting and selection bias.
set.seed(1, sample.kind="Rounding")
control <- trainControl(method = "cv", number = 10, p = .8)

# In the following steps, the selected models are fitted first. Afterwards
# predictions are made as part of an sapply statement. Finally, we will
# evaluate the results using different performance metrics.
# Attention: This takes several minutes or an hour depending on your hardware.

# Model Training & Prediction
set.seed(1, sample.kind="Rounding")
predictions <- sapply(models, function(model) {
  print(model)
  fit <- train(Attrition_Flag ~ ., method = model, trControl = control, data = train_set)
  prediction <- predict(fit, test_set)
  data.frame(model = prediction)
})

# Transform predictions into data frame
predictions <- as.data.frame(predictions)

# Inspect predictions
View(predictions)


## 2.5 Model Evaluation
# We evaluate the predictions and compute a confusion matrix for each model.
# The confusion matrix is a table that describes the performance of a 
# classification model with certain figures. From it you can derive several 
# parameters, i.e. accuracy, sensitivity, specificity and prevalence among 
# others. We will use multiple metrics to evaluate our models to get a clearer 
# picture of the models' performances. We will use the "Accuracy" and the 
# "F-measure" to determine the performance of our models.

# Theory: The "Accuracy" is the number of correct predictions in relation to 
# total predictions made. It is one of the most common performance measures.

# Furthermore, we will use the "F1 score" or "F-measure". 
# Theory: It is a number between 0 and 1. It is the harmonic mean of "Precision" 
# and "Recall", I.e. it balances precision and recall and is also a common 
# measure for the evaluation of classification models.

# Theory: Precision (or Pos. Pred Value) is the share of correct positive 
# predictions (True Positives) in relation to the total positive predictions 
# (True Positives + False Positives).

# Theory: Recall (or Specificity) is the share of correct positive predictions 
# (True Positives) in relation to the total positives (True Positives + False 
# Negatives). 
# Source: https://towardsdatascience.com/the-5-classification-evaluation-metrics-you-must-know-aa97784ff226

# We compute the accuracy for the used models.
accuracies <- sapply(predictions, function(x) {
  confusionMatrix(data=x, reference=test_set$Attrition_Flag)$overall["Accuracy"]
})
print(accuracies)

# The models have very high accuracies, the bank manager would be happy to 
# see the results. When looking at the numbers, we see that the "AdaBoost" 
# model has the highest accuracy.
print(accuracies[which.max(accuracies)])

# Let us now look at the F_measures for the used models.
f_measures <- sapply(predictions, function(x) {
  F_meas(data=x, reference=test_set$Attrition_Flag)
})
print(f_measures)

# The "AdaBoost" model also has the highest F-measure.
print(f_measures[which.max(f_measures)])

# As of now, it looks like the AdaBoost model is the best model to choose.
# However, we can also check whether an ensemble of the selected models will 
# give us better results than the AdaBoost model alone. To build an ensemble, 
# we observe the predictions by each model and determine a "majority vote", i.e.
# the prediction of the ensemble model is the prediction that the majority of
# the underlying models make.

# To compute the majority vote, we can use the rowMeans() function and check
# for the prediction "Attributed Customer". 
votes <- rowMeans(predictions == "Attrited Customer")

# Inspect majority votes.
# If the vote is > 0.5, it means that more than 50% of the predictions have
# votes for "Attrited Customer".
View(votes)

# Prediction with ensemble and majority vote.
predEnsemble <- as.factor(ifelse(votes > 0.5, "Attrited Customer", "Existing Customer"))

# Evaluation of the ensemble model using accuracy and F-measure.
accuracyEnsemble <- confusionMatrix(data=predEnsemble, reference=test_set$Attrition_Flag)$overall["Accuracy"]
print(accuracyEnsemble)

fMeasureEnsemble <- F_meas(data=predEnsemble, reference=test_set$Attrition_Flag)
print(fMeasureEnsemble)

# Evaluation of Ensemble Model
# Although the ensemble model performs also quite well, it is not better than 
# the "AdaBoost" model alone. Hence, we will select the "AdaBoost" model as our 
# final model. 

# Optimization
# Let us take a closer look into the "AdaBoost" model to see what else we can 
# find out. We will apply a slightly larger tuning grid to see if we can improve 
# our model further. However we should avoid optimizing it too much since this 
# would result in overfitting the model, and depending on the hardware, would 
# take a very very long time.

# Attention: This optimization step with a tuning grid from 0 to 1000 in 50' steps
# will take very long (~15-20 hours) depending on your machine. Having a more 
# granular parameter search e.g. from 1 to 1000 in 1 steps would take extremely 
# long and would most probably result in overfitting. Hence, we will not do that.
# Feel free to run it if you want, otherwise please skip this line of code.
#fitAdaboost <- train(Attrition_Flag ~ ., method = "adaboost", trControl = control, tuneGrid = data.frame(nIter = seq(0, 1000, 50), method = "Adaboost.M1"), data = train_set)

# From my personal trials (yes I ran it few times), I found that the optimal 
# number of trees is approximately 450 with the "Adaboost.M1" method. 
# Knowing the optimal number of trees, we can massively accelerate the fitting
# process, and tell R to use nIter = 450 with method = "AdaBoost.M1", i.e.:
fitAdaboost <- train(Attrition_Flag ~ ., method = "adaboost", trControl = control, tuneGrid = data.frame(nIter = 450, method = "Adaboost.M1"), data = train_set)

# Inspect the final model parameters.
# We see the optimal model parameters, e.g. nIter = 450 and method = "Adaboost.M1"
# for the optimized model. It means, the model uses 450 trees to decide whether
# a customer is going to churn or not.
fitAdaboost
fitAdaboost$bestTune

# Now, we quickly check how the optimized model performs in terms of accuracy 
# and F-measure. Therefore we compute the same metrics as before.
predAdaboost <- predict(fitAdaboost, test_set)

accuracyAdaboost <- confusionMatrix(data=predAdaboost, reference=test_set$Attrition_Flag)$overall["Accuracy"]
print(accuracyAdaboost)

fMeasureAdaboost <- F_meas(data=predAdaboost, reference=test_set$Attrition_Flag)
print(fMeasureAdaboost)

# Indeed, the optimized AdaBoost model has improved further. 

## 3. Results
# Next, we check the variable importance to gather further insights and to see 
# which variables in the data set have the highest explanatory power. This 
# information can be very valuable and useful to identify potentially churning 
# customers at an early stage and to initiate early countermeasures.
varImp(fitAdaboost)

# From this, we see the most important variables in the data set to predict
# potentially churning customers, e.g. the number of transactions in the last 
# 12 months, the change in the number of transactions in the last 12 months, or
# the total revolving balance on the credit card. Those should be of special 
# importance and serve as early warning indicators to the bank manager, since 
# they are indicators of a potentially churning customer.


## 4. Summary and Limitations
# From our evaluation results, we see that the "Adaboost" model has the highest 
# accuracy and the highest F-measure. It means, the "AdaBoost" model correctly 
# predicted approximately 97% of the "Attrited Customers". Even after using
# the F-measure, which is a more balanced metric, we see that the "AdaBoost"
# model is still superior to the other selected models for the dataset at hand.
# For the "Bank Churners" use case we have, our insights can be very useful
# for the bank manager. He could use the model to predict which customer is 
# likely to churn, and hence proactively contact the customer to offer an
# extended service or special credit card conditions.

# Limitations
# Since the underlying data set only has a relatively small prevalence of ~16% 
# of "Attrited Customers", it may not be solid and reliable enough to have 
# very robust predictions of churning customers. Hence, one way to improve or
# to stabilize the model would be gather more data at the bank itsel, or to buy 
# additional data from external data providers. Nonetheless, the "AdaBoost" 
# model performs very well with the available dataset and serve as a basis for 
# further development.