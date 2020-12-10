#Prerequisites
options(digits = 3)
library(dslabs)

data(heights)
heights

str(heights)

heights$height[777]

heights$sex[777]
heights[777,1]

heights$height[which.max(heights$height)]

heights$height[which.min(heights$height)]

mean(heights$height)
median(heights$height)

library(tidyverse)

heights %>% filter(height > 78 & sex == "Female") %>% group_by(sex) %>% summarise(n=n()) %>% mutate(prop= n/sum(n))

##Notation
#Key points
#X1,...,Xp  denote the features,  Y  denotes the outcomes, and  Y^  denotes the predictions.
#Machine learning prediction tasks can be divided into categorical and continuous outcomes. We refer to these as classification and prediction, respectively.

##Example
#Key points
#Yi  = an outcome for observation or index i.
#We use boldface for  X_i  to distinguish the vector of predictors from the individual predictors  Xi,1,...,Xi,784 .
#When referring to an arbitrary set of features and outcomes, we drop the index i and use  Y  and bold  X .
#Uppercase is used to refer to variables because we think of predictors as random variables.
#Lowercase is used to denote observed values. For example,  X=x .

##Caret
#Key points
#Note: the set.seed() function is used to obtain reproducible results. If you have R 3.6 or later, please use the sample.kind = "Rounding" argument whenever you set the seed for this course.
#To mimic the ultimate evaluation process, we randomly split our data into two — a training set and a test set — and act as if we don’t know the outcome of the test set. We develop algorithms using only the training set; the test set is used only for evaluation.
#The createDataPartition()  function from the caret package can be used to generate indexes for randomly splitting data.
#Note: contrary to what the documentation says, this course will use the argument p as the percentage of data that goes to testing. The indexes made from createDataPartition() should be used to create the test set. Indexes should be created on the outcome and not a predictor.
#The simplest evaluation metric for categorical outcomes is overall accuracy: the proportion of cases that were correctly predicted in the test set.