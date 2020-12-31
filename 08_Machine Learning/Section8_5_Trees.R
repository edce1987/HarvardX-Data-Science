#Key points
#LDA and QDA are not meant to be used with many predictors  p  because the number of parameters needed to be estimated becomes too large.
#Curse of dimensionality: For kernel methods such as kNN or local regression, when they have multiple predictors used,  the span/neighborhood/window made to include a given percentage of the data become large. With larger neighborhoods, our methods lose flexibility. The dimension here refers to the fact that when we have  p  predictors, the distance between two observations is computed in  p -dimensional space.

#Key points
#A tree is basically a flow chart of yes or no questions. The general idea of the methods we are describing is to define an algorithm that uses data to create these trees with predictions at the ends, referred to as nodes.
#When the outcome is continuous, we call the decision tree method a regression tree.
#Regression and decision trees operate by predicting an outcome variable  Y  by partitioning the predictors.
#The general idea here is to build a decision tree and, at end of each node, obtain a predictor  y^ . Mathematically, we are partitioning the predictor space into  J  non-overlapping regions,  R1  ,  R2 , ...,  RJ  and then for any predictor  x  that falls within region  Rj , estimate  f(x)  with the average of the training observations  yi  for which the associated predictor  xi  in also in  Rj .
#To pick  j  and its value  s , we find the pair that minimizes the residual sum of squares (RSS):
#  ∑i:xiR1(j,s)(yi−y^R1)2+∑i:xiR2(j,s)(yi−y^R2)2 
#To fit the regression tree model, we can use the rpart() function in the rpart package.
#Two common parameters used for partition decision are the complexity parameter (cp) and the minimum number of observations required in a partition before partitioning it further (minsplit in the rpart package). 
#If we already have a tree and want to apply a higher cp value, we can use the prune() function. We call this pruning a tree because we are snipping off partitions that do not meet a cp criterion. 
#Code
# Load data
library(tidyverse)
library(dslabs)
data("olive")
olive %>% as_tibble()
table(olive$region)
olive <- select(olive, -area)

# Predict region using KNN
library(caret)
fit <- train(region ~ .,  method = "knn", 
             tuneGrid = data.frame(k = seq(1, 15, 2)), 
             data = olive)
ggplot(fit)

# Plot distribution of each predictor stratified by region
olive %>% gather(fatty_acid, percentage, -region) %>%
  ggplot(aes(region, percentage, fill = region)) +
  geom_boxplot() +
  facet_wrap(~fatty_acid, scales = "free") +
  theme(axis.text.x = element_blank())

# plot values for eicosenoic and linoleic
p <- olive %>% 
  ggplot(aes(eicosenoic, linoleic, color = region)) + 
  geom_point()
p + geom_vline(xintercept = 0.065, lty = 2) + 
  geom_segment(x = -0.2, y = 10.54, xend = 0.065, yend = 10.54, color = "black", lty = 2)

# load data for regression tree
data("polls_2008")
qplot(day, margin, data = polls_2008)

library(rpart)
fit <- rpart(margin ~ ., data = polls_2008)

# visualize the splits 
plot(fit, margin = 0.1)
text(fit, cex = 0.75)
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# change parameters
fit <- rpart(margin ~ ., data = polls_2008, control = rpart.control(cp = 0, minsplit = 2))
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# use cross validation to choose cp
library(caret)
train_rpart <- train(margin ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), data = polls_2008)
ggplot(train_rpart)

# access the final model and plot it
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)
polls_2008 %>% 
  mutate(y_hat = predict(train_rpart)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# prune the tree 
pruned_fit <- prune(fit, cp = 0.01)
train_rpart$finalModel

#Key points
#Classification trees, or decision trees, are used in prediction problems where the outcome is categorical. 
#Decision trees form predictions by calculating which class is the most common among the training set observations within the partition, rather than taking the average in each partition.
#Two of the more popular metrics to choose the partitions are the Gini index and entropy.
#Gini(j)=∑k=1Kp^j,k(1−p^j,k) 
#entropy(j)=−∑k=1Kp^j,klog(p^j,k),with 0×log(0)defined as 0 
#Pros: Classification trees are highly interpretable and easy to visualize.They can model human decision processes and don’t require use of dummy predictors for categorical variables.
#Cons: The approach via recursive partitioning can easily over-train and is therefore a bit harder to train than. Furthermore, in terms of accuracy, it is rarely the best performing method since it is not very flexible and is highly unstable to changes in training data. 
#Code
# fit a classification tree and plot it
train_rpart <- train(y ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = mnist_27$train)
plot(train_rpart)

# compute accuracy
confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

####Random Forest
#Key points
#Random forests are a very popular machine learning approach that addresses the shortcomings of decision trees. The goal is to improve prediction performance and reduce instability by averaging multiple decision trees (a forest of trees constructed with randomness).
#The general idea of random forests is to generate many predictors, each using regression or classification trees, and then forming a final prediction based on the average prediction of all these trees. To assure that the individual trees are not the same, we use the bootstrap to induce randomness. 
#A disadvantage of random forests is that we lose interpretability.
#An approach that helps with interpretability is to examine variable importance. To define variable importance we count how often a predictor is used in the individual trees. The caret package includes the function varImp that extracts variable importance from any model in which the calculation is implemented. 
#Code
library(randomForest)
fit <- randomForest(margin~., data = polls_2008) 
plot(fit)

polls_2008 %>%
  mutate(y_hat = predict(fit, newdata = polls_2008)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_line(aes(day, y_hat), col="red")

library(randomForest)
train_rf <- randomForest(y ~ ., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

# use cross validation to choose parameter
train_rf_2 <- train(y ~ .,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                    data = mnist_27$train)
confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

####Assessment
library(rpart)
n <- 1000
sigma <- 0.25
# set.seed(1) # if using R 3.5 or ealier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

#Q1
fit <- rpart(y ~ ., data = dat)

#Q2
plot(fit)
text(fit, cex = 0.75)

#Q3
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)

#Q4
library(randomForest)
fit <- randomForest(y ~ x, data = dat) 
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

#Q5
plot(fit)

#Q6
library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

####Caret
#Key points
#The caret package helps provides a uniform interface and standardized syntax for the many different machine learning packages in R. Note that caret does not automatically install the packages needed.
#Code
library(tidyverse)
library(dslabs)
data("mnist_27")

library(caret)
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]

####
#Key points
#The train() function automatically uses cross-validation to decide among a few default values of a tuning parameter.
#The getModelInfo() and modelLookup() functions can be used to learn more about a model and the parameters that can be optimized.
#We can use the tunegrid() parameter in the train() function to select a grid of values to be compared.
#The trControl parameter and trainControl() function can be used to change the way cross-validation is performed.
#Note that not all parameters in machine learning algorithms are tuned. We use the train() function to only optimize parameters that are tunable.
#Code
getModelInfo("knn")
modelLookup("knn")
modelLookup()

train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)
ggplot(train_knn, highlight = TRUE)

train_knn <- train(y ~ ., method = "knn", 
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))
ggplot(train_knn, highlight = TRUE)
train_knn$bestTune
train_knn$finalModel
confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall["Accuracy"]

control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(y ~ ., method = "knn", 
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = TRUE)

train_knn$results %>% 
  ggplot(aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = k, 
                    ymin = Accuracy - AccuracySD,
                    ymax = Accuracy + AccuracySD))

plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5),color="black")
}

plot_cond_prob(predict(train_knn, mnist_27$true_p, type = "prob")[,2])

install.packages("gam")
modelLookup("gamLoess")

grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)

train_loess <- train(y ~ ., 
                     method = "gamLoess",
                     tuneGrid=grid,
                     data = mnist_27$train)
ggplot(train_loess, highlight = TRUE)

confusionMatrix(data = predict(train_loess, mnist_27$test), 
                reference = mnist_27$test$y)$overall["Accuracy"]

p1 <- plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])
p1

####Assessment
#Q1
library(rpart)
library(dslabs)

dataset <- data.frame(tissue_gene_expression)
?train
set.seed(1991, sample.kind = "Rounding")
fit <- train(y ~ ., method="rpart", data = dataset, tuneGrid = data.frame(cp=seq(0,0.1,0.01)))
ggplot(fit)
fit

#or
library(caret)
library(rpart)          
library(dslabs)
# set.seed(1991) # if using R 3.5 or earlier
set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
data("tissue_gene_expression")

fit <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))
fit
ggplot(fit)

#Q2
dataset <- data.frame(tissue_gene_expression)

set.seed(1991, sample.kind = "Rounding")
fit <- train(y ~ ., method="rpart", data = dataset, tuneGrid = data.frame(cp=seq(0,0.1,0.01)), control=rpart.control(minsplit = 0))
ggplot(fit)
fit

#or
library(caret)                    
library(rpart)
library(dslabs)
data("tissue_gene_expression")

# set.seed(1991) # if using R 3.5 or earlier
set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later

fit_rpart <- with(tissue_gene_expression, 
                  train(x, y, method = "rpart",
                        tuneGrid = data.frame(cp = seq(0, 0.10, 0.01)),
                        control = rpart.control(minsplit = 0)))
ggplot(fit_rpart)
confusionMatrix(fit_rpart)

#Q3
plot(fit_rpart$finalModel)
text(fit_rpart$finalModel)

#Q4
set.seed(1991, sample.kind = "Rounding")
mtry <- seq(50, 200, 25)
tg <- expand.grid(.mtry=mtry)
fit <- train(y ~ ., method="rf", data = dataset, nodesize = 1, tuneGrid = tg)
ggplot(fit)

fit$bestTune

confusionMatrix(fit)

#Q5
imp <- varImp(fit)
imp

#Q6
imp <- varImp(fit_rpart)
tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms

data_frame(term = rownames(imp$importance), 
           importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
  filter(term %in% tree_terms)

####Titanic I
install.packages("titanic")
library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

#Q1
set.seed(42, sample.kind = "Rounding")
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
test_set <- titanic_clean[test_index, ]
train_set <- titanic_clean[-test_index, ]

train_set %>% select(Survived) %>% summarise(mean(Survived==1))

#Q2
set.seed(3, sample.kind = "Rounding")
guessing <- data.frame(Survived=as.factor(sample(c(0,1), size = 179, replace = TRUE)))

confusionMatrix(data=guessing$Survived, reference=test_set$Survived)

#or
set.seed(3, sample.kind = "Rounding")
# guess with equal probability of survival
guess <- sample(c(0,1), nrow(test_set), replace = TRUE)
mean(guess == test_set$Survived)

#Q3a
train_set %>% group_by(Sex) %>% summarise(m=mean(Survived==1))

#Q3b
test_set %>% group_by(Sex) %>% summarise(n=n(), Survival=mean(Survived==1))

sex_based <- test_set %>% mutate(prediction = ifelse(Sex=="male", 0, 1))
mean(sex_based$prediction == test_set$Survived)

#or
sex_model <- ifelse(test_set$Sex == "female", 1, 0)    # predict Survived=1 if female, 0 if male
mean(sex_model == test_set$Survived)    # calculate accuracy

#Q4a
train_set %>% group_by(Pclass) %>% summarise(mean(Survived==1))

#Q4b
pclass_based <- test_set %>% mutate(prediction = ifelse(Pclass==1, 1, 0))
mean(pclass_based$prediction == test_set$Survived)

#or
class_model <- ifelse(test_set$Pclass == 1, 1, 0)    # predict survival only if first class
mean(class_model == test_set$Survived)    # calculate accuracy

#Q4c
train_set %>% group_by(Sex, Pclass) %>% summarise(mean(Survived==1))

#Q4d
psclass_based <- test_set %>% mutate(prediction = ifelse(Pclass==1 & Sex =="female" | Pclass ==2 & Sex=="female", 1, 0))
mean(psclass_based$prediction == test_set$Survived)

#or
sex_class_model <- ifelse(test_set$Sex == "female" & test_set$Pclass != 3, 1, 0)
mean(sex_class_model == test_set$Survived)

#Q5a
confusionMatrix(data=factor(sex_based$prediction), reference=factor(test_set$Survived))
confusionMatrix(data=factor(pclass_based$prediction), reference=factor(test_set$Survived))
confusionMatrix(data=factor(psclass_based$prediction), reference=factor(test_set$Survived))

#Q5b
0.806

#Q6
F_meas(data=factor(sex_based$prediction), reference=factor(test_set$Survived))
F_meas(data=factor(pclass_based$prediction), reference=factor(test_set$Survived))
F_meas(data=factor(psclass_based$prediction), reference=factor(test_set$Survived))

#Q7
set.seed(1, sample.kind="Rounding")
fit_lda <- train(Survived ~ Fare, method = "lda", data = train_set)
y_hat <- predict(fit_lda, test_set)
confusionMatrix(data = y_hat, reference = test_set$Survived)$overall["Accuracy"]

set.seed(1, sample.kind="Rounding")
fit_qda <- train(Survived ~ Fare, method = "qda", data = train_set)
y_hat <- predict(fit_qda, test_set)
confusionMatrix(data = y_hat, reference = test_set$Survived)$overall["Accuracy"]

#or
set.seed(1, sample.kind = "Rounding")
train_lda <- train(Survived ~ Fare, method = "lda", data = train_set)
lda_preds <- predict(train_lda, test_set)
mean(lda_preds == test_set$Survived)

set.seed(1, sample.kind = "Rounding")
train_qda <- train(Survived ~ Fare, method = "qda", data = train_set)
qda_preds <- predict(train_qda, test_set)
mean(qda_preds == test_set$Survived)

#Q8
set.seed(1, sample.kind = "Rounding")
fit_glm <- train(Survived ~ Age, method = "glm", data = train_set)
y_hat <- predict(fit_glm, test_set)
confusionMatrix(data = y_hat, reference = test_set$Survived)$overall["Accuracy"]

set.seed(1, sample.kind = "Rounding")
fit_glm <- train(Survived ~ Sex + Pclass + Fare + Age, method = "glm", data = train_set)
y_hat <- predict(fit_glm, test_set)
confusionMatrix(data = y_hat, reference = test_set$Survived)$overall["Accuracy"]

set.seed(1, sample.kind = "Rounding")
fit_glm <- train(Survived ~ ., method = "glm", data = train_set)
y_hat <- predict(fit_glm, test_set)
confusionMatrix(data = y_hat, reference = test_set$Survived)$overall["Accuracy"]

#Q9a
modelLookup("knn")
tg <- data.frame(k=seq(3,51,2))
set.seed(6, sample.kind = "Rounding")
fit_knn <- train(Survived ~ ., method = "knn", data = train_set, tuneGrid = tg)
plot(fit_knn)
fit_knn$bestTune


#or
set.seed(6, sample.kind = "Rounding") # if using R 3.6 or later
train_knn <- train(Survived ~ .,
                   method = "knn",
                   data = train_set,
                   tuneGrid = data.frame(k = seq(3, 51, 2)))
train_knn$bestTune

#Q9b
11

#Q9c
y_hat <- predict(fit_knn, test_set)
confusionMatrix(data = y_hat, reference = test_set$Survived)$overall["Accuracy"]

#Q10
set.seed(8, sample.kind = "Rounding")
control <- trainControl(method = "cv", number = 10, p = .9)
fit_knn_cv <- train(Survived ~ ., method = "knn", 
                      data = train_set,
                      tuneGrid = data.frame(k = seq(3, 51, 2)),
                      trControl = control)
ggplot(fit_knn_cv, highlight = TRUE)

y_hat <- predict(fit_knn_cv, test_set)
confusionMatrix(data = y_hat, reference = test_set$Survived)$overall["Accuracy"]

#Q11a
set.seed(10, sample.kind = "Rounding")
fit_rpart <- train(Survived ~ ., method = "rpart", 
                    data = train_set,
                    tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))
ggplot(fit_rpart, highlight = TRUE)
fit_rpart

y_hat <- predict(fit_rpart, test_set)
confusionMatrix(data = y_hat, reference = test_set$Survived)$overall["Accuracy"]

#Q11b
fit_rpart$finalModel
ggplot(fit_rpart)
plot(fit_rpart$finalModel)
text(fit_rpart$finalModel)

#Q12
set.seed(14, sample.kind = "Rounding")
fit_rf <- train(Survived ~ ., method = "rf", 
                   data = train_set,
                   tuneGrid = data.frame(mtry = seq(1:7)), ntree = 100)
ggplot(fit_rf, highlight = TRUE)
fit_rf

y_hat <- predict(fit_rf, test_set)
confusionMatrix(data = y_hat, reference = test_set$Survived)$overall["Accuracy"]

imp <- varImp(fit_rf)
imp

