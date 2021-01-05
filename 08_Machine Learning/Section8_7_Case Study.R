options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)
View(high)

x <- brca$x
y <- brca$y

#Q1
dim(brca$x)[1]
dim(brca$x)[2]
mean(brca$y == "M")
which.max(colMeans(brca$x))
which.min(colSds(brca$x))

#Q2
?sweep
x <- sweep(x, 2, colMeans(x), "-")
x <- sweep(x, 2, colSds(x), "/")

sd(x[,1])
median(x[,1])

#Q3
?dist

d_samples <- dist(x)
dist_BtoB <- as.matrix(d_samples)[1, brca$y == "B"]
mean(dist_BtoB[2:length(dist_BtoB)])

dist_BtoM <- as.matrix(d_samples)[1, brca$y == "M"]
mean(dist_BtoM)

#Q4
?t
?heatmap
d_features <- dist(t(x))
heatmap(as.matrix(d_features))

#Q5
hcl <- hclust(d_samples)
plot(hcl)
cc <- cutree(hcl, 5)
cc
plot(cc)

#or
h <- hclust(d_features)
groups <- cutree(h, k = 5)
split(names(groups), groups)

####Assessment
#Q6
pca <- prcomp(x)

dim(pca$rotation)

dim(pca$x)

plot(pca$sdev)

var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
var_explained
plot(var_explained)

#or
pca <- prcomp(x)
summary(pca)     # first value of Cumulative Proportion that exceeds 0.9: PC7

#Q7
pc <- pca
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

#or
data.frame(pca$x[,1:2], type = brca$y) %>%
  ggplot(aes(PC1, PC2, color = type)) +
  geom_point()

#Q8
data.frame(type = brca$y, pca$x[,1:10]) %>%
  gather(key = "PC", value = "value", -type) %>%
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot()

####Assessment
set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x[test_index,]
test_y <- brca$y[test_index]
train_x <- x[-test_index,]
train_y <- brca$y[-test_index]

#Q9
mean(train_y=="B")
mean(test_y=="B")

#Q10a
predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}

library(caret)
set.seed(3, sample.kind = "Rounding")    # if using R 3.6 or later
k <- kmeans(train_x, centers = 2)
kmeans_preds <- ifelse(predict_kmeans(test_x, k) == 1, "B", "M")
mean(kmeans_preds == test_y)

#Q10b
table(test_y, kmeans_preds)

sensitivity(factor(kmeans_preds), test_y, positive = "B")
sensitivity(factor(kmeans_preds), test_y, positive = "M")

#Q11
fit <- train(train_x, train_y, method = "glm")
glm_preds <- predict(fit, test_x)
confusionMatrix(data = y_hat, reference = test_y)$overall["Accuracy"]

#or
train_glm <- train(train_x, train_y, method = "glm")
glm_preds <- predict(train_glm, test_x)
mean(glm_preds == test_y)

#Q12
train_lda <- train(train_x, train_y, method = "lda")
lda_preds <- predict(train_lda, test_x)
mean(lda_preds == test_y)

train_qda <- train(train_x, train_y, method = "qda")
qda_preds <- predict(train_qda, test_x)
mean(qda_preds == test_y)

#Q13
library(gam)

train_loess <- train(train_x, train_y , 
                     method = "gamLoess")
gamLoess_preds <- predict(train_loess, test_x)
mean(gamLoess_preds == test_y)

#or
set.seed(5, sample.kind = "Rounding") # simulate R 3.5
train_loess <- train(train_x, train_y, method = "gamLoess")
loess_preds <- predict(train_loess, test_x)
mean(loess_preds == test_y)

####
#Q14
set.seed(7, sample.kind = "Rounding")
k <- seq(3,21,1)
train_knn <- train(train_x, train_y, method = "knn", tuneGrid = data.frame(k))
knn_preds <- predict(train_knn, test_x)
mean(knn_preds == test_y)
train_knn$bestTune

#or
knn_preds <- predict(train_knn, test_x)
mean(knn_preds == test_y)

#Q15a
set.seed(9, sample.kind = "Rounding")
mtry <- c(3,5,7,9)
train_rf <- train(train_x, train_y, method = "rf", tuneGrid = data.frame(mtry), importance = TRUE)
rf_preds <- predict(train_rf, test_x)
mean(rf_preds == test_y)
train_rf$bestTune
train_rf$finalModel$importance
varImp(train_rf)

#Q16a
models <- c("K means", "glm", "lda", "qda", "gamLoess", "knn", "rf")

fits <- lapply(models, function(model){ 
  print(model)
  train(train_x, train_y, method = model)
}) 

names(fits) <- models
fits

preds <- sapply(fits, function(fit){
  predict(fit, test_x)
})
dim(preds)
View(preds)
class(preds)
acc <- colMeans(pred == mnist_27$test$y)
acc
mean(acc)
?rbind
#Q4

votes <- rowMeans(preds == "M")
y_hat <- ifelse(votes > 0.5, "M", "B")
mean(y_hat == test_y)

#or
ensemble <- cbind(glm = glm_preds == "B", lda = lda_preds == "B", qda = qda_preds == "B", loess = loess_preds == "B", rf = rf_preds == "B", knn = knn_preds == "B", kmeans = kmeans_preds == "B")

ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, "B", "M")
mean(ensemble_preds == test_y)

#Q16b
models <- c("K means", "Logistic regression", "LDA", "QDA", "Loess", "K nearest neighbors", "Random forest", "Ensemble")
accuracy <- c(mean(kmeans_preds == test_y),
              mean(glm_preds == test_y),
              mean(lda_preds == test_y),
              mean(qda_preds == test_y),
              mean(loess_preds == test_y),
              mean(knn_preds == test_y),
              mean(rf_preds == test_y),
              mean(ensemble_preds == test_y))
data.frame(Model = models, Accuracy = accuracy)