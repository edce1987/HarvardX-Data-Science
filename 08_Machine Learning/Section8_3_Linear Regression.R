#Key points
#Linear regression can be considered a machine 
#learning algorithm. Although it can be too rigid 
#to be useful, it works rather well for some 
#challenges. It also serves as a baseline 
#approach: if you can’t beat it with a more complex 
#approach, you probably want to stick to linear 
#regression. 

library(tidyverse)
library(HistData)

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

library(caret)

y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

avg <- mean(train_set$son)
avg

mean((avg - test_set$son)^2)

# fit linear regression model
fit <- lm(son ~ father, data = train_set)
fit$coef
summary(fit)

y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)

####

#Key points
#The predict() function takes a fitted 
#object from functions such as 
#lm() or glm() and a data frame with the new 
#predictors for which to predict. We can use 
#predict like this:
y_hat <- predict(fit, test_set)

#predict() is a generic function in R that 
#calls other functions depending on what kind 
#of object it receives. To learn about the 
#specifics, you can read the help files using 
#code like this: 
?predict.lm    # or ?predict.glm

y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)

# read help files
?predict.lm
?predict.glm

####Assessment
options(digits=3)
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

#Q1
n <- 100
x <- dat$x
y <- dat$y
set.seed(1, sample.kind="Rounding")
sim <- replicate(n, {
  test_index <- createDataPartition(y , times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  rmse <- sqrt(mean((y_hat-test_set$y)^2))
})
mean(sim)
sd(sim)

#Q2
n <- c(100, 500, 1000, 5000, 10000)
gen <- function(n) {
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>% data.frame() %>% setNames(c("x", "y"))
  sim <- replicate(100, {
    test_index <- createDataPartition(dat$y , times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)
    rmse <- sqrt(mean((y_hat-test_set$y)^2))
  })
  output <- c(mean(sim), sd(sim))
}

set.seed(1, sample.kind="Rounding")
res <- sapply(n,gen)
res

#Q4
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

n <- 100
x <- dat$x
y <- dat$y
set.seed(1, sample.kind="Rounding")
sim <- replicate(n, {
  test_index <- createDataPartition(y , times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  rmse <- sqrt(mean((y_hat-test_set$y)^2))
})
mean(sim)
sd(sim)

#Q6

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(dat$y , times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)
y <- dat$y
x_1 <- dat$x_1
x_2 <- dat$x_2
fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2)) #RMSE

#Q8
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1, sample.kind="Rounding")

####Categorical Outcomes
options(digits=3)
library(tidyverse)
library(dslabs)
library(caret)

data("heights")
y <- heights$height

set.seed(2, sample.kind = "Rounding") #if you are using R 3.6 or later

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

train_set %>% 
  filter(round(height)==66) %>%
  summarize(y_hat = mean(sex=="Female"))

heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point()
lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>% lm(y ~ height, data = .)
p_hat <- predict(lm_fit, test_set)
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat, test_set$sex)$overall["Accuracy"]

####Logistic Regression
#Key points
#Logistic regression is an extension of linear regression that assures that the estimate of conditional probability  Pr(Y=1|X=x)  is between 0 and 1. This approach makes use of the logistic transformation: 
#  g(p)=logp1−p 
#With logistic regression, we model the conditional probability directly with:
#  g{Pr(Y=1|X=x)}=β0+β1x 
#Note that with this model, we can no longer use least squares. Instead we compute the maximum likelihood estimate (MLE). 
#In R, we can fit the logistic regression model with the function glm() (generalized linear models). If we want to compute the conditional probabilities, we want type="response" since the default is to return the logistic transformed values.

heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point() + 
  geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2])

range(p_hat)

# fit logistic regression model
glm_fit <- train_set %>% 
  mutate(y = as.numeric(sex == "Female")) %>%
  glm(y ~ height, data=., family = "binomial")

p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")

tmp <- heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) 
logistic_curve <- data.frame(x = seq(min(tmp$x), max(tmp$x))) %>%
  mutate(p_hat = plogis(glm_fit$coef[1] + glm_fit$coef[2]*x))
tmp %>% 
  ggplot(aes(x, prop)) +
  geom_point() +
  geom_line(data = logistic_curve, mapping = aes(x, p_hat), lty = 2)

y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor
confusionMatrix(y_hat_logit, test_set$sex)$overall[["Accuracy"]]

####2 or 7
#Key points
#In this case study we apply logistic regression to classify whether a digit is two or seven. We are interested in estimating a conditional probability that depends on two variables:
#  g{p(x1,x2}=g{Pr(Y=1|X1=x1,X2=x2)}=β0+β1x1+β2x2 
#Through this case, we know that logistic regression forces our estimates to be a plane and our boundary to be a line. This implies that a logistic regression approach has no chance of capturing the non-linear nature of the true  p(x1,x2) . Therefore, we need other more flexible methods that permit other shapes.

mnist <- read_mnist()
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1), which.max(mnist_27$train$x_1))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

data("mnist_27")
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

is <- mnist_27$index_train[c(which.min(mnist_27$train$x_2), which.max(mnist_27$train$x_2))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")
p_hat_glm <- predict(fit_glm, mnist_27$test)
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 7, 2))
confusionMatrix(data = y_hat_glm, reference = mnist_27$test$y)$overall["Accuracy"]

mnist_27$true_p %>% ggplot(aes(x_1, x_2, fill=p)) +
  geom_raster()

mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black") 

p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black") 

p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") +
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test)

##Assessment
set.seed(1, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1=2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

set.seed(1, sample.kind="Rounding")
delta <- seq(0, 3, len = 25)
res <- sapply(delta, function(d){
  dat <- make_data(mu_1 = d)
  fit_glm <- dat$train %>% glm(y ~ x, family = "binomial", data = .)
  y_hat_glm <- ifelse(predict(fit_glm, dat$test) > 0.5, 1, 0) %>% factor(levels = c(0, 1))
  mean(y_hat_glm == dat$test$y)
})
qplot(delta, res)

####Smoothing
#Key points
#Smoothing is a very powerful technique used all across data analysis. It is designed to detect trends in the presence of noisy data in cases in which the shape of the trend is unknown. 
#The concepts behind smoothing techniques are extremely useful in machine learning because conditional expectations/probabilities can be thought of as trends of unknown shapes that we need to estimate in the presence of uncertainty.

library(dslabs)
library(HistData)
library(tidyverse)

data("polls_2008")
qplot(day, margin, data = polls_2008)

#Key points
#The general idea of smoothing is to group data points into strata in which the value of  f(x)  can be assumed to be constant. We can make this assumption because we think  f(x)  changes slowly and, as a result,  f(x)  is almost constant in small windows of time. 
#This assumption implies that a good estimate for  f(x)  is the average of the  Yi  values in the window. The estimate is:
#  f^(x0)=1N0∑i∈A0Yi 
#In smoothing, we call the size of the interval  |x−x0|  satisfying the particular condition the window size, bandwidth or span.

# bin smoothers
span <- 7 
fit <- with(polls_2008,ksmooth(day, margin, x.points = day, kernel="box", bandwidth =span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

# kernel
span <- 7
fit <- with(polls_2008, ksmooth(day, margin,  x.points = day, kernel="normal", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

####
#Key points
#A limitation of the bin smoothing approach is that we need small windows for the approximately constant assumptions to hold which may lead to imprecise estimates of  f(x) . Local weighted regression (loess) permits us to consider larger window sizes.
#One important difference between loess and bin smoother is that we assume the smooth function is locally linear in a window instead of constant.
#The result of loess is a smoother fit than bin smoothing because we use larger sample sizes to estimate our local parameters.

polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() + 
  geom_smooth(color="red", span = 0.15, method = "loess", method.args = list(degree=1))

##Assessment Smoothing
library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")

dat %>% ggplot(aes(date, deaths)) + 
  geom_point() + 
  geom_smooth(color="red", span = 0.05, method = "loess", method.args = list(degree=1))


#Q1
span <- 60 / as.numeric(diff(range(dat$date)))
fit <- dat %>% mutate(x = as.numeric(date)) %>% loess(deaths ~ x, data = ., span = span, degree = 1)
dat %>% mutate(smooth = predict(fit, as.numeric(date))) %>%
  ggplot() +
  geom_point(aes(date, deaths)) +
  geom_line(aes(date, smooth), lwd = 2, col = "red")

#Q2
dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)

#Q3
library(broom)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()

qplot(x_2, y, data = mnist_27$train)

A <- mnist_27$train

A %>% mutate(y_t = ifelse(y==7, 1, 0)) %>% ggplot(aes(x_2, y)) + geom_point() + 
  geom_smooth(color="red", span = 0.01, method = "loess", method.args = list(degree=1))

##Matrices
#Key points
#The main reason for using matrices is that certain mathematical operations needed to develop efficient code can be performed using techniques from a branch of mathematics called linear algebra.
#Linear algebra and matrix notation are key elements of the language used in academic papers describing machine learning techniques.

library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()

class(mnist$train$images)

x <- mnist$train$images[1:1000,] 
y <- mnist$train$labels[1:1000]

####

#Key points
#In matrix algebra, we have three main types of objects: scalars, vectors, and matrices.
#Scalar:   α=1 
#Vector:   X1=⎛⎝⎜⎜x1,1⋮xN,1⎞⎠⎟⎟ 
#Matrix:  X=[X1X2]=⎛⎝⎜⎜x1,1⋮xN,1x1,2⋮xN,2⎞⎠⎟⎟ 
#In R, we can extract the dimension of a matrix with the function dim(). We can convert a vector into a matrix using the function as.matrix().

length(x[,1])
x_1 <- 1:5
x_2 <- 6:10
cbind(x_1, x_2)
dim(x)
dim(x_1)
dim(as.matrix(x_1))
dim(x)

####
#Key points
#In R, we can convert a vector into a matrix with the matrix() function. The matrix is filled in by column, but we can fill by row by using the byrow argument. The function t() can be used to directly transpose a matrix. 
#Note that the matrix function recycles values in the vector without warning if the product of columns and rows does not match the length of the vector.

my_vector <- 1:15

# fill the matrix by column
mat <- matrix(my_vector, 5, 3)
mat

# fill by row
mat_t <- matrix(my_vector, 3, 5, byrow = TRUE)
mat_t
identical(t(mat), mat_t)
matrix(my_vector, 5, 5)
grid <- matrix(x[3,], 28, 28)
image(1:28, 1:28, grid)

# flip the image back
image(1:28, 1:28, grid[, 28:1])


####
#Key points
#The function rowSums() computes the sum of each row.
#The function rowMeans() computes the average of each row.
#We can compute the column sums and averages using the functions colSums() and colMeans().
#The matrixStats package adds functions that performs operations on each row or column very efficiently, including the functions rowSds() and colSds().
#The apply() function lets you apply any function to a matrix. The first argument is the matrix, the second is the dimension (1 for rows, 2 for columns), and the third is the function. 

sums <- rowSums(x)
avg <- rowMeans(x)

data_frame(labels = as.factor(y), row_averages = avg) %>%
  qplot(labels, row_averages, data = ., geom = "boxplot")

avgs <- apply(x, 1, mean)
sds <- apply(x, 2, sd)

install.packages("matrixStats")

##
#Key points
#The operations used to extract columns: x[,c(351,352)].
#The operations used to extract rows: x[c(2,3),].
#We can also use logical indexes to determine which columns or rows to keep:  new_x <- x[ ,colSds(x) > 60].
#Important note: if you select only one column or only one row, the result is no longer a matrix but a vector. We can preserve the matrix class by using the argument drop=FALSE. 

library(matrixStats)
library(tidyverse)

sds <- colSds(x)
qplot(sds, bins = "30", color = I("black"))
image(1:28, 1:28, matrix(sds, 28, 28)[, 28:1])

#extract columns and rows
x[ ,c(351,352)]
x[c(2,3),]
new_x <- x[ ,colSds(x) > 60]
dim(new_x)
class(x[,1])
dim(x[1,])

#preserve the matrix class
class(x[ , 1, drop=FALSE])
dim(x[, 1, drop=FALSE])

####

mat <- matrix(1:15, 5, 3)
mat[mat > 6 & mat < 12] <- 0

bin_x <- x
bin_x[bin_x < 255/2] <- 0 
bin_x[bin_x > 255/2] <- 1

#index with matrices
mat <- matrix(1:15, 5, 3)
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

#binarize the data
bin_x <- x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1
bin_X <- (x > 255/2)*1

####
#Key points
#We can scale each row of a matrix using this line of code:
(x - rowMeans(x)) / rowSds(x)
#To scale each column of a matrix, we use this code:
t(t(x) - colMeans(x))
#We can also use a function called sweep() that works similarly to apply(). It takes each entry of a vector and subtracts it from the corresponding row or column:
X_mean_0 <- sweep(x, 2, colMeans(x))
#Matrix multiplication: t(x) %*% x
#The cross product: crossprod(x)
#The inverse of a function: solve(crossprod(x))
#The QR decomposition: qr(x)
  
#scale each row of a matrix
(x - rowMeans(x)) / rowSds(x)
  
#scale each column
t(t(x) - colMeans(x))
  
#take each entry of a vector and subtracts it from the corresponding row or column
x_mean_0 <- sweep(x, 2, colMeans(x))
  
#divide by the standard deviation
x_mean_0 <- sweep(x, 2, colMeans(x))
x_standardized <- sweep(x_mean_0, 2, colSds(x), FUN = "/")

####Assessment Matrices

#Q1
x <- matrix(rnorm(100*10), 100, 10)
dim(x)

#Q2
nrow(x)
ncol(x)

#Q3
x <- x + seq(nrow(x))
x <- sweep(x, 1, 1:nrow(x),"+")

#)Q4
x <- sweep(x, 2, 1:ncol(x), FUN = "+")

#Q5
rowMeans(x)

#Q6
mnist <- read_mnist()
y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
qplot(as.factor(mnist$train$labels), y, geom = "boxplot")
mean(y) # proportion of pixels

####

