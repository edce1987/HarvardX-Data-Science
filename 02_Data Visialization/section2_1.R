data(murders)
head(murders)

library(dslabs)
data(heights)

head(heights)

prop.table(table(heights$sex))

hist(heights$height, )
help("hist")

#Key points
#A distribution is a function or description that shows the possible values of a variable and how often those values occur.
#For categorical variables, the distribution describes the proportions of each category.
#A frequency table is the simplest way to show a categorical distribution. Use prop.table() to convert a table of counts to a frequency table. Barplots display the distribution of categorical variables and are a way to visualize the information in frequency tables.
#For continuous numerical data, reporting the frequency of each unique entry is not an effective summary as many or most values are unique. Instead, a distribution function is required.
#The cumulative distribution function (CDF) is a function that reports the proportion of data below a value  𝑎  for all values of  𝑎 :  𝐹(𝑎)=Pr(𝑥≤𝑎) .
#The proportion of observations between any two values  𝑎  and  𝑏  can be computed from the CDF as  𝐹(𝑏)−𝐹(𝑎) .
#A histogram divides data into non-overlapping bins of the same size and plots the counts of number of values that fall in that interval.

a <- seq(min(my_data), max(my_data), length = 100)    # define range of values spanning the dataset
cdf_function <- function(x) {    # computes prob. for a single value
  mean(my_data <= x)
}
cdf_values <- sapply(a, cdf_function)
plot(a, cdf_values)

library(dslabs)
data(heights)
quantile(heights$height, seq(.01, 0.99, 0.01))

library(dslabs)
data(heights)
x <- heights$height[heights$sex == "Male"]
mean(x > 69 & x <= 72)

library(dslabs)
data(heights)
x <- heights$height[heights$sex=="Male"]
avg <- mean(x)
stdev <- sd(x)
pnorm(72,avg,stdev) - pnorm(69, avg,stdev)

# define x as vector of male heights
library(tidyverse)
library(dslabs)
data(heights)
index <- heights$sex=="Male"
x <- heights$height[index]

# calculate the mean and standard deviation manually
average <- sum(x)/length(x)
SD <- sqrt(sum((x - average)^2)/length(x))

# built-in mean and sd functions - note that the audio and printed values disagree
average <- mean(x)
SD <- sd(x)
c(average = average, SD = SD)

# calculate standard units
z <- scale(x)

# calculate proportion of values within 2 SD of mean
mean(abs(z) < 2)

#Standard units
#For data that are approximately normal, standard units describe the number of standard deviations an observation is from the mean. Standard units are denoted by the variable  𝑧  and are also known as z-scores.

#For any value  𝑥  from a normal distribution with mean  𝜇  and standard deviation  𝜎 , the value in standard units is:
  
# 𝑧=𝑥−𝜇𝜎 
#Standard units are useful for many reasons. Note that the formula for the normal distribution is simplified by substituting  𝑧  in the exponent:
  
#  Pr(𝑎<𝑥<𝑏)=∫𝑏𝑎12𝜋√𝜎𝑒−12𝑧2𝑑𝑥 
#When  𝑧=0 , the normal distribution is at a maximum, the mean  𝜇 . The function is defined to be symmetric around  𝑧=0 .

#The normal distribution of z-scores is called the standard normal distribution and is defined by  𝜇=0  and  𝜎=1 .

#Z-scores are useful to quickly evaluate whether an observation is average or extreme. Z-scores near 0 are average. Z-scores above 2 or below -2 are significantly above or below the mean, and z-scores above 3 or below -3 are extremely rare. 

#We will learn more about benchmark z-score values and their corresponding probabilities below.

#Code: Converting to standard units
#The scale function converts a vector of approximately normally distributed values into z-scores.

#z <- scale(x)
#You can compute the proportion of observations that are within 2 standard deviations of the mean like this:
  
 # mean(abs(z) < 2)
#Annotate

library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
#We can estimate the probability that a male is taller than 70.5 inches with:
  
  1 - pnorm(70.5, mean(x), sd(x))
#Code: Discretization and the normal approximation
# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))

#Definition of quantiles
#Quantiles are cutoff points that divide a dataset into intervals with set probabilities. The  𝑞 th quantile is the value at which  𝑞 % of the observations are equal to or less than that value.

#Using the quantile function
#Given a dataset data and desired quantile q, you can find the qth quantile of data with:
  
  quantile(data,q)
#Percentiles
#Percentiles are the quantiles that divide a dataset into 100 intervals each with 1% probability. You can determine all percentiles of a dataset data like this:
  
  p <- seq(0.01, 0.99, 0.01)
quantile(data, p)
#Quartiles
#Quartiles divide a dataset into 4 parts each with 25% probability. They are equal to the 25th, 50th and 75th percentiles. The 25th percentile is also known as the 1st quartile, the 50th percentile is also known as the median, and the 75th percentile is also known as the 3rd quartile.

T#he summary() function returns the minimum, quartiles and maximum of a vector.

#Examples
#Load the heights dataset from the dslabs package:
  
  library(dslabs)
data(heights)
#Use summary() on the heights$height variable to find the quartiles:
  
  summary(heights$height)
#Find the percentiles of heights$height:
  
  p <- seq(0.01, 0.99, 0.01)
percentiles <- quantile(heights$height, p)
#Confirm that the 25th and 75th percentiles match the 1st and 3rd quartiles. Note that quantile() returns a named vector. You can access the 25th and 75th percentiles like this (adapt the code for other percentile values):
  
  percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]

#Definition of qnorm
#The qnorm() function gives the theoretical value of a quantile with probability p of observing a value equal to or less than that quantile value given a normal distribution with mean mu and standard deviation sigma:
  
  qnorm(p, mu, sigma)
#By default, mu=0 and sigma=1. Therefore, calling qnorm() with no arguments gives quantiles for the standard normal distribution.

qnorm(p)
#Recall that quantiles are defined such that p is the probability of a random observation less than or equal to the quantile.

#Relation to pnorm
#The pnorm() function gives the probability that a value from a standard normal distribution will be less than or equal to a z-score value z. Consider:
  
#  pnorm(-1.96)  ≈0.025 
#The result of pnorm() is the quantile. Note that:
  
#  qnorm(0.025)  ≈−1.96 
#qnorm() and pnorm() are inverse functions:
  
  pnorm(qnorm(0.025))  =0.025 
#Theoretical quantiles
#You can use qnorm() to determine the theoretical quantiles of a dataset: that is, the theoretical value of quantiles assuming that a dataset follows a normal distribution. Run the qnorm() function with the desired probabilities p, mean mu and standard deviation sigma. 

#Suppose male heights follow a normal distribution with a mean of 69 inches and standard deviation of 3 inches. The theoretical quantiles are:
  
  p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles <- qnorm(p, 69, 3)
#Theoretical quantiles can be compared to sample quantiles determined with the quantile function in order to evaluate whether the sample follows a normal distribution.

mean(x<= 69.5)

#QQ Plot
p <- seq(0.05, 0.95, 0.05)

observed_quantiles <- quantile(x,p)
theoretical_quantiles <- qnorm(p, mean=mean(x), sd=sd(x))
plot(observed_quantiles, theoretical_quantiles)
abline(0,1)

#With standard units
observed_quantiles <- quantile(z,p)
theoretical_quantiles <- qnorm(p)
plot(observed_quantiles, theoretical_quantiles)
abline(0,1)

mean(x)
sd(x)


