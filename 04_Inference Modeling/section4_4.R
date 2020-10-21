library(tidyverse)

d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d+1)/2

# calculate confidence intervals of the spread
confidence_intervals <- sapply(Ns, function(N){
  X <- sample(c(0,1), size=N, replace=TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat) - 1
})

# generate a data frame storing results
polls <- data.frame(poll = 1:ncol(confidence_intervals),
                    t(confidence_intervals), sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls

d_hat <- polls %>%
  summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>%
  .$avg

p_hat <- (1+d_hat)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))   
round(d_hat*100,1)
round(moe*100, 1)

####

#Key points
#We analyze real 2016 US polling data organized by FiveThirtyEight. We start by using reliable national polls taken within the week before the election to generate an urn model.
#Consider  p  the proportion voting for Clinton and  1−p  the proportion voting for Trump. We are interested in the spread  d=2p−1 .
#Poll results are a random normal variable with expected value of the spread  d  and standard error  sqrt(2p(1−p)/N)−−−−−−−−−−√ .
#Our initial estimate of the spread did not include the actual spread. Part of the reason is that different pollsters have different numbers of polls in our dataset, and each pollster has a bias.
#Pollster bias reflects the fact that repeated polls by a given pollster have an expected value different from the actual spread and different from other pollsters. Each pollster has a different bias.
#The urn model does not account for pollster bias. We will develop a more flexible data-driven model that can account for effects like bias.

library(dslabs)
library(ggplot2)
data("polls_us_election_2016")
names(polls_us_election_2016)

polls <- polls_us_election_2016 %>% filter(state=="U.S." & enddate >= "2016-10-31" & (grade %in% c("A+", "A", "A-","B+") | is.na(grade)))
           
polls <- polls %>% mutate(spread=rawpoll_clinton/100 - rawpoll_trump/100)

d_hat <- polls %>% summarize(d_hat=sum(spread*samplesize)/sum(samplesize)) %>% .$d_hat
                                           
p_hat <- (d_hat+1)/2

moe <- 1.96*2*sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))
moe

polls %>% ggplot(aes(spread)) + geom_histogram(binwidth = 0.01, color="black")

polls %>% group_by(pollster) %>% filter(n()>=6) %>% ggplot(aes(pollster, spread)) + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# standard errors within each pollster
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  summarize(se = 2 * sqrt(p_hat * (1-p_hat) / median(samplesize)))

####

#Key points
#Instead of using an urn model where each poll is a random draw from the same distribution of voters, we instead define a model using an urn that contains poll results from all possible pollsters.
#We assume the expected value of this model is the actual spread  d=2p−1 .
#Our new standard error  σ  now factors in pollster-to-pollster variability. It can no longer be calculated from  p  or  d  and is an unknown parameter.
#The central limit theorem still works to estimate the sample average of many polls  X1,...,XN  because the average of the sum of many random variables is a normally distributed random variable with expected value  d  and standard error  σ/sqrt(N) .
#We can estimate the unobserved  σ  as the sample standard deviation, which is calculated with the sd function.

# collect last result before the election for each pollster
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%      # keep latest poll
  ungroup()

# histogram of spread estimates
one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)

# construct 95% confidence interval
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
round(results*100, 1)

####

# The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
head(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X <- sample(x, N, replace = TRUE)

# Define `se` as the standard error of the estimate. Print this value to the console.
se <- sd(X)/sqrt(N)
se

# Construct a 95% confidence interval for the population average based on our sample. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(mean(X) - qnorm(0.975)*se, mean(X) + qnorm(0.975)*se)

?between

####

# Define `mu` as the population average
mu <- mean(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `B` as the number of times to run the model
B <- 10000

# Define an object `res` that contains a logical vector for simulated intervals that contain mu
res <- replicate(B, {
  X <- sample(x, N, replace=TRUE)
  se <- sd(X)/sqrt(N)
  interval <- c(mean(X)-qnorm(0.975)*se,mean(X)+qnorm(0.975)*se)
  between(mu, interval[1], interval[2])
}
)

# Calculate the proportion of results in `res` that include mu. Print this value to the console.
mean(res)

####

# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)

# Create an object called `sigma` that contains a column for `pollster` and a column for `s`, the standard deviation of the spread
sigma <- polls %>% group_by(pollster) %>% summarize(s = sd(spread))

# Print the contents of sigma to the console
sigma

####

# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)

# Create an object called `res` that summarizes the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% group_by(pollster) %>% summarize(mean=mean(spread),sd=sd(spread),N=n())


# Store the difference between the larger average and the smaller in a variable called `estimate`. Print this value to the console.
estimate <- res$mean[2] - res$mean[1]
estimate

# Store the standard error of the estimates as a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(res$sd[2]^2/res$N[2] + res$sd[1]^2/res$N[1])
se_hat

# Calculate the 95% confidence interval of the spreads. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(estimate-qnorm(0.975)*se_hat, estimate + qnorm(0.975)*se_hat)
res

####

# We made an object `res` to summarize the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% group_by(pollster) %>% summarize(avg = mean(spread), s = sd(spread), N = n()) 

# The variables `estimate` and `se_hat` contain the spread estimates and standard error, respectively.
estimate <- res$avg[2] - res$avg[1]
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])

# Calculate the p-value
2 * (1 - pnorm(estimate / se_hat, 0, 1))



