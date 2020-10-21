#Key points
#We can use statistical theory to compute the probability that a given interval contains the true parameter  𝑝 .
#95% confidence intervals are intervals constructed to have a 95% chance of including  𝑝 . The margin of error is approximately a 95% confidence interval.
#The start and end of these confidence intervals are random variables.
#To calculate any size confidence interval, we need to calculate the value  𝑧  for which  Pr(−𝑧≤𝑍≤𝑧)  equals the desired confidence. For example, a 99% confidence interval requires calculating  𝑧  for  Pr(−𝑧≤𝑍≤𝑧)=0.99 .
#For a confidence interval of size  𝑞 , we solve for  𝑧=1−1−𝑞2 .
#To determine a 95% confidence interval, use z <- qnorm(0.975). This value is slightly smaller than 2 times the standard error.

data("nhtemp")
data.frame(year = as.numeric(time(nhtemp)), temperature = as.numeric(nhtemp)) %>%
  ggplot(aes(year, temperature)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average Yearly Temperatures in New Haven")

##

p <- 0.45
N <- 1000

X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p,p))
X_hat <- mean(X)
SE_hat <- sqrt(X_hat*(1-X_hat)/N)
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)

z <- qnorm(0.995)

pnorm(qnorm(0.995))

pnorm(qnorm(1-0.995))

pnorm(z) - pnorm(-z)

#1-(1-q)/2

qnorm(0.975)

#Key points
#We can use statistical theory to compute the probability that a given interval contains the true parameter  𝑝 .
#95% confidence intervals are intervals constructed to have a 95% chance of including  𝑝 . The margin of error is approximately a 95% confidence interval.
#The start and end of these confidence intervals are random variables.
#To calculate any size confidence interval, we need to calculate the value  𝑧  for which  Pr(−𝑧≤𝑍≤𝑧)  equals the desired confidence. For example, a 99% confidence interval requires calculating  𝑧  for  Pr(−𝑧≤𝑍≤𝑧)=0.99 .
#For a confidence interval of size  𝑞 , we solve for  𝑧=1−1−𝑞2 .
#To determine a 95% confidence interval, use z <- qnorm(0.975). This value is slightly smaller than 2 times the standard error.

####MC for conf. interval

B <- 10000
N <- 1000
p <- 0.45
inside <- replicate(B, {
  X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p,p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat - 2*SE_hat, X_hat + 2*SE_hat)
})
mean(inside)
inside

####

N <- 25
X_hat <- 0.48
(2*X_hat - 1) + c(-2, 2)*2*sqrt(X_hat*(1-X_hat)/sqrt(N))

####Pvalue
#Key points
#The null hypothesis is the hypothesis that there is no effect. In this case, the null hypothesis is that the spread is 0, or  𝑝=0.5 .
#The p-value is the probability of detecting an effect of a certain size or larger when the null hypothesis is true.
#We can convert the probability of seeing an observed value under the null hypothesis into a standard normal random variable. We compute the value of  𝑧  that corresponds to the observed result, and then use that  𝑧  to compute the p-value.
#If a 95% confidence interval does not include our observed value, then the p-value must be smaller than 0.05.
#It is preferable to report confidence intervals instead of p-values, as confidence intervals give information about the size of the estimate and p-values do not.

N <- 100    # sample size
z <- sqrt(N) * 0.02/0.5    # spread of 0.02
1 - (pnorm(z) - pnorm(-z))

#The p-value is the probability of observing a value as extreme or more extreme than the result given that the null hypothesis is true. 

#In the context of the normal distribution, this refers to the probability of observing a Z-score whose absolute value is as high or higher than the Z-score of interest.

#Suppose we want to find the p-value of an observation 2 standard deviations larger than the mean. This means we are looking for anything with  ∣𝑧∣≥2 .  ∣z∣≥2 

#Graphically, the p-value gives the probability of an observation that's at least as far away from the mean or further. This plot shows a standard normal distribution (centered at  z=0  with a standard deviation of 1). The shaded tails are the region of the graph that are 2 standard deviations or more away from the mean.

#The p-value is the proportion of area under a normal curve that has z-scores as extreme or more extreme than the given value - the tails on this plot of a normal distribution are shaded to show the region corresponding to the p-value.

#The right tail can be found with 1-pnorm(2). We want to have both tails, though, because we want to find the probability of any observation as far away from the mean or farther, in either direction. (This is what's meant by a two-tailed p-value.) Because the distribution is symmetrical, the right and left tails are the same size and we know that our desired value is just 2*(1-pnorm(2)).

#Recall that, by default, pnorm() gives the CDF for a normal distribution with a mean of  𝜇=0  and standard deviation of  𝜎=1 . To find p-values for a given z-score z in a normal distribution with mean mu and standard deviation sigma, use 2*(1-pnorm(z, mu, sigma)) instead.

##DataCamp

# Load the data
data(polls_us_election_2016)

# Generate an object `polls` that contains data filtered for polls that ended on or after October 31, 2016 in the United States
polls <- polls_us_election_2016 %>% filter(enddate>='2016-10-31' & state=="U.S.")

# How many rows does `polls` contain? Print this value to the console.
nrow(polls)

# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- first(polls$samplesize)
N

# For the first poll in `polls`, assign the estimated percentage of Clinton voters to a variable called `X_hat`. Print this value to the console.
X_hat <- first(polls$rawpoll_clinton)/100
X_hat
# Calculate the standard error of `X_hat` and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(X_hat*(1-X_hat)/N)
se_hat
# Use `qnorm` to calculate the 95% confidence interval for the proportion of Clinton voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(X_hat-qnorm(.975)*se_hat,X_hat+qnorm(.975)*se_hat)
ci

####

pollster_results <- polls %>% 
  mutate(X_hat=rawpoll_clinton/100, se_hat=sqrt(X_hat*(1-X_hat)/samplesize), lower=X_hat-qnorm(.975)*se_hat, upper=X_hat+qnorm(.975)*se_hat) %>% 
  select(pollster, enddate, X_hat, se_hat, lower, upper)

####

# The `pollster_results` object has already been loaded. Examine it using the `head` function.
head(pollster_results)

# Add a logical variable called `hit` that indicates whether the actual value exists within the confidence interval of each poll. Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called `avg_hit`.
avg_hit <- pollster_results %>% mutate(hit=0.482 >= lower & 0.482 < upper) %>% summarize(mean(hit))

####

# Add a statement to this line of code that will add a new column named `d_hat` to `polls`. The new column should contain the difference in the proportion of voters.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") %>% mutate(d_hat=(rawpoll_clinton-rawpoll_trump)/100)

# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls$samplesize[1]
N
# Assign the difference `d_hat` of the first poll in `polls` to a variable called `d_hat`. Print this value to the console.
d_hat <- polls$d_hat[1]
d_hat
# Assign proportion of votes for Clinton to the variable `X_hat`.
X_hat <- (d_hat+1)/2
# Calculate the standard error of the spread and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- 2*sqrt(X_hat*(1-X_hat)/N)

# Use `qnorm` to calculate the 95% confidence interval for the difference in the proportions of voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(d_hat-qnorm(.975)*se_hat,d_hat+qnorm(.975)*se_hat)

####

# The subset `polls` data with 'd_hat' already calculated has been loaded. Examine it using the `head` function.
head(polls)

# Create a new object called `pollster_results` that contains columns for pollster name, end date, d_hat, lower confidence interval of d_hat, and upper confidence interval of d_hat for each poll.
pollster_results <- polls %>% 
  mutate(X_hat=(d_hat+1)/2, se_hat=2*sqrt(X_hat*(1-X_hat)/samplesize), lower=d_hat-qnorm(.975)*se_hat, upper=d_hat+qnorm(.975)*se_hat) %>% 
  select(pollster, enddate, d_hat, lower, upper)

####

# The `pollster_results` object has already been loaded. Examine it using the `head` function.
head(pollster_results)

# Add a logical variable called `hit` that indicates whether the actual value (0.021) exists within the confidence interval of each poll. Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called `avg_hit`.
avg_hit <- pollster_results %>% mutate(hit=0.021 >= lower & 0.021 < upper) %>% summarize(mean(hit))

####

# The `polls` object has already been loaded. Examine it using the `head` function.
head(polls)

# Add variable called `error` to the object `polls` that contains the difference between d_hat and the actual difference on election day. Then make a plot of the error stratified by pollster, but only for pollsters who took 5 or more polls.
polls %>% mutate(error=d_hat-0.021) %>% group_by(pollster) %>% filter(n() >= 5) %>% ggplot(aes(error,pollster)) + geom_point()

####



