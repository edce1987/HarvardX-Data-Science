# HarvardX: PH125.8x
# Data Science: Machine Learning
# R code from course videos

# Linear Regression for Prediction, Smoothing, and Working with Matrices

## Working with Matrices

### Matrices

library(tidyverse)
library(dslabs)
mnist <- read_mnist()

class(mnist$train$images)

x <- mnist$train$images[1:1000,] 
y <- mnist$train$labels[1:1000]

### Matrix Notation

length(x[,1])

x_1 <- 1:5
x_2 <- 6:10
cbind(x_1, x_2)

dim(x)

dim(x_1)
dim(as.matrix(x_1))

dim(x)

### Converting a Vector to a Matrix

my_vector <- 1:15
mat <- matrix(my_vector, 5, 3)
mat

mat_t <- matrix(my_vector, 3, 5, byrow = TRUE)
mat_t

identical(t(mat), mat_t)

matrix(my_vector, 5, 5)

grid <- matrix(x[3,], 28, 28)

image(1:28, 1:28, grid)
image(1:28, 1:28, grid[, 28:1])

### Row and Column Summaries and Apply

sums <- rowSums(x)

avg <- rowMeans(x)

data_frame(labels = as.factor(y), row_averages = avg) %>% 
     qplot(labels, row_averages, data = ., geom = "boxplot") 

avgs <- apply(x, 1, mean)
sds <- apply(x, 2, sd)

### Filtering Columns Based on Summaries

library(matrixStats)
sds <- colSds(x)
qplot(sds, bins = "30", color = I("black"))

image(1:28, 1:28, matrix(sds, 28, 28)[, 28:1])

x[ ,c(351,352)]

x[c(2,3),]

new_x <- x[ ,colSds(x) > 60]
dim(new_x)

class(x[,1])
dim(x[1,])

class(x[ , 1, drop=FALSE])
dim(x[, 1, drop=FALSE])

### Indexing with Matrices and Binarizing the Data

mat <- matrix(1:15, 5, 3)
mat
as.vector(mat)

qplot(as.vector(x), bins = 30, color = I("black"))

new_x <- x
new_x[new_x < 50] <- 0

mat <- matrix(1:15, 5, 3)
mat[mat < 3] <- 0
mat

mat <- matrix(1:15, 5, 3)
mat[mat > 6 & mat < 12] <- 0
mat

bin_x <- x
bin_x[bin_x < 255/2] <- 0 
bin_x[bin_x > 255/2] <- 1

bin_X <- (x > 255/2)*1

rafalib::mypar(1,2)
rows <- 1:28
columns <- 1:28
image(rows, columns, matrix(-x[8,], 28, 28), main = "Original")
image(rows, columns, matrix(-bin_x[8,], 28, 28), main ="Binarized")

### Vectorization for Matrices and Matrix Algebra Operations

(x - rowMeans(x)) / rowSds(x)

t(t(x) - colMeans(x))

X_mean_0 <- sweep(x, 2, colMeans(x))

x_mean_0 <- sweep(x, 2, colMeans(x))
x_standardized <- sweep(x_mean_0, 2, colSds(x), FUN = "/")

t(x) %*% x
crossprod(x)

solve(crossprod(x))

qr(x)
