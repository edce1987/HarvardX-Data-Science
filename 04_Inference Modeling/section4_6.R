#Key points
#In our model:
#The spread  d∼N(μ,τ)  describes our best guess in the absence of polling data. We set  μ=0  and  τ=0.035  using historical data.
#The average of observed data  X¯∣d∼N(d,σ)  describes randomness due to sampling and the pollster effect.
#Because the posterior distribution is normal, we can report a 95% credible interval that has a 95% chance of overlapping the parameter using  E(p∣Y)  and  SE(p∣Y) .
#Given an estimate of  E(p∣Y)  and  SE(p∣Y) , we can use pnorm to compute the probability that  d>0 .
#It is common to see a general bias that affects all pollsters in the same way. This bias cannot be predicted or measured before the election. We will include a term in later models to account for this variability.

library(tidyverse)
library(dslabs)
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()

results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)

mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

posterior_mean
posterior_se

# 95% credible interval
posterior_mean + c(-1.96, 1.96)*posterior_se

# probability of d > 0
1 - pnorm(0, posterior_mean, posterior_se)

####

#Key points
#If we collect several polls with measured spreads  X1,...,Xj  with a sample size of  N , these random variables have expected value  d  and standard error  2*sqrt(p(1−p)/N).
#We represent each measurement as  Xi,j=d+b+hi+ϵi,j  where:
#  The index  i  represents the different pollsters
#The index  j  represents the different polls
#Xi,j  is the  j th poll by the  i th pollster 
#d  is the actual spread of the election
#b  is the general bias affecting all pollsters
#hi  represents the house effect for the  i th pollster
#ϵi,j  represents the random error associated with the  i,j th poll.
#The sample average is now  X¯=d+b+1/N*∑i=1N Xi  with standard deviation  SE(X¯)=sqrt(σ2/N+σ2b).
#The standard error of the general bias  σb  does not get reduced by averaging multiple polls, which increases the variability of our final estimate.

J <- 6
N <- 2000
d <- 0.021
p <- (d+1)/2
X <- d + rnorm(J,0,2*sqrt(p*(1-p)/N))

I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})

I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
h <- rnorm(I, 0, 0.025)    # assume standard error of pollster-to-pollster variability is 0.025
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})

mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + .025^2)
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

1 - pnorm(0, posterior_mean, posterior_se)

####

#Key points
#In the US election, each state has a certain number of votes that are won all-or-nothing based on the popular vote result in that state (with minor exceptions not discussed here).
#We use the left_join() function to combine the number of electoral votes with our poll results.
#For each state, we apply a Bayesian approach to generate an Election Day  d . We keep our prior simple by assuming an expected value of 0 and a standard deviation based on recent history of 0.02.
#We can run a Monte Carlo simulation that for each iteration simulates poll results in each state using that state's average and standard deviation, awards electoral votes for each state to Clinton if the spread is greater than 0, then compares the number of electoral votes won to the number of votes required to win the election (over 269).
#If we run a Monte Carlo simulation for the electoral college without accounting for general bias, we overestimate Clinton's chances of winning at over 99%.
#If we include a general bias term, the estimated probability of Clinton winning decreases significantly.

library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
head(results_us_election_2016)

results_us_election_2016 %>% arrange(desc(electoral_votes)) %>% top_n(5, electoral_votes)

results <- polls_us_election_2016 %>%
  filter(state!="U.S." &
           !grepl("CD", state) &
           enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-","B+") | is.na(grade))) %>%
  mutate(spread=rawpoll_clinton/100-rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg=mean(spread), sd=sd(spread), n=n()) %>%
  mutate(state = as.character(state))

# 10 closest races = battleground states
results %>% arrange(abs(avg))

# joining electoral college votes and results
results <- left_join(results, results_us_election_2016, by="state")

# states with no polls: note Rhode Island and District of Columbia = Democrat
results_us_election_2016 %>% filter(!state %in% results$state)

# assigns sd to states with just one poll as median of other sd values
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))

mu <- 0
tau <- 0.02
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n),
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV > 269)    # over 269 votes wins election

# histogram of outcomes
data.frame(clinton_EV) %>%
  ggplot(aes(clinton_EV)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)

mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/(n) + bias_sd^2),    # added bias_sd term
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV_2 > 269)    # over 269 votes wins election

####

#Key points
#In poll results,  p  is not fixed over time. Variability within a single pollster comes from time variation.
#In order to forecast, our model must include a bias term  bt  to model the time effect.
#Pollsters also try to estimate  f(t) , the trend of  p  given time  t  using a model like:
#  Yi,j,t=d+b+hj+bt+f(t)+ϵi,j,t 
#Once we decide on a model, we can use historical data and current data to estimate the necessary parameters to make predictions.

# select all national polls by one pollster
one_pollster <- polls_us_election_2016 %>%
  filter(pollster == "Ipsos" & state == "U.S.") %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# the observed standard error is higher than theory predicts
se <- one_pollster %>%
  summarize(empirical = sd(spread),
            theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
se

# the distribution of the data is not normal
one_pollster %>% ggplot(aes(spread)) + geom_histogram(binwidth = 0.01, color = "black")

polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) +
  geom_smooth(method = "loess", span = 0.1) +
  geom_point(aes(color = pollster), show.legend = FALSE, alpha = 0.6)

polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color = candidate)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30, 50))

####

# Load the libraries and data
library(dplyr)
library(dslabs)
data("polls_us_election_2016")

# Create a table called `polls` that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>% filter(state != "U.S." & enddate >= "2016-10-31") %>% mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
polls
# Create an object called `cis` that has the columns indicated in the instructions
cis <- polls %>% mutate(X_hat = (spread+1)/2, se = 2*sqrt(X_hat*(1-X_hat)/samplesize), 
                        lower = spread - qnorm(0.975)*se, upper = spread + qnorm(0.975)*se) %>%
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)

####

# Add the actual results to the `cis` data set
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
ci_data
# Create an object called `p_hits` that summarizes the proportion of confidence intervals that contain the actual value. Print this object to the console.
# p_hits <- ci_data %>% mutate(hit= actual_spread>=lower & actual_spread < upper) %>% summarize(hitrate=mean(hit))
p_hits

?between

####

# The `cis` data have already been loaded for you
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of hits for each pollster that has at least 5 polls.
p_hits <- ci_data %>% mutate(hit=actual_spread >= lower & actual_spread < upper) %>% group_by(pollster) %>% filter(n()>=5) %>% summarize(proportion_hits=mean(hit), n=n(), grade=first(grade)) %>% arrange(desc(proportion_hits))
p_hits

####

# The `cis` data have already been loaded for you
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of hits for each state that has more than 5 polls.
p_hits <- ci_data %>% mutate(hit=actual_spread>=lower & actual_spread<=upper) %>% group_by(state) %>% filter(n()>5) %>% summarize(proportion_hits=mean(hit), n=n()) %>% arrange(desc(proportion_hits))
p_hits

####

# The `cis` data have already been loaded. Examine it using the `head` function.
head(cis)

# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors <- cis %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))

# Examine the last 6 rows of `errors`
tail(errors)
head(errors)

####

# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors <- cis %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))

# Create an object called `p_hits` that summarizes the proportion of hits for each state that has 5 or more polls
p_hits <- errors %>% group_by(state) %>% filter(n()>=5) %>% summarize(proportion_hits=mean(hit), n=n())




# Make a barplot of the proportion of hits for each state
p_hits %>% mutate(state = reorder(state, proportion_hits)) %>% ggplot(aes(state,proportion_hits)) + geom_bar(stat="identity") + coord_flip()

####

# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)

# Create a boxplot showing the errors by state for polls with grades B+ or higher
errors %>% filter(grade %in% c("A+","A","A-","B+")) %>% mutate(state = reorder(state,error)) %>% ggplot(aes(state,error)) + geom_boxplot() + geom_point()

####t-distribution

#Key points
#In models where we must estimate two parameters,  𝑝  and  𝜎 , the Central Limit Theorem can result in overconfident confidence intervals for sample sizes smaller than approximately 30.
#If the population data are known to follow a normal distribution, theory tells us how much larger to make confidence intervals to account for estimation of  𝜎 .
#Given  𝑠  as an estimate of  𝜎 , then  𝑍=𝑋¯−𝑑/sigma*sqrt(𝑁)  follows a t-distribution with  𝑁−1  degrees of freedom.
#Degrees of freedom determine the weight of the tails of the distribution. Small values of degrees of freedom lead to increased probabilities of extreme values.
#We can determine confidence intervals using the t-distribution instead of the normal distribution by calculating the desired quantile with the function qt().
library(dslabs)
data(one_poll_per_pollster)
z <- qt(0.975, nrow(one_poll_per_pollster)-1)
one_poll_per_pollster %>% summarize(avg=mean(spread), moe=z*sd(spread)/sqrt(length(spread))) %>% mutate(start=avg-moe, end=avg+moe)

qt(0.975,14)

####

# Calculate the probability of seeing t-distributed random variables being more than 2 in absolute value when 'df = 3'.
1-pt(2,3)+pt(-2,3)

####

# Generate a vector 'df' that contains a sequence of numbers from 3 to 50
df <- seq(3,50,1)

# Make a function called 'pt_func' that calculates the probability that a value is more than |2| for any degrees of freedom 
pt_func <- function(df){
  1-pt(2,df)+pt(-2,df)
}

# Generate a vector 'probs' that uses the `pt_func` function to calculate the probabilities
probs <- sapply(df, pt_func)

# Plot 'df' on the x-axis and 'probs' on the y-axis
plot(df,probs)

####

# Load the neccessary libraries and data
library(dslabs)
library(dplyr)
data(heights)

# Use the sample code to generate 'x', a vector of male heights
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Create variables for the mean height 'mu', the sample size 'N', and the number of times the simulation should run 'B'
mu <- mean(x)
N <- 15
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a logical vector 'res' that contains the results of the simulations
res <- replicate(B, {
  X <- sample(x, N, replace=TRUE)
  interval <- mean(X) + c(-1,1)*qnorm(0.975)*sd(X)/sqrt(N)
  between(mu, interval[1], interval[2])
})

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
mean(res)

####

# The vector of filtered heights 'x' has already been loaded for you. Calculate the mean.
mu <- mean(x)

# Use the same sampling parameters as in the previous exercise.
set.seed(1)
N <- 15
B <- 10000

# Generate a logical vector 'res' that contains the results of the simulations using the t-distribution
res <- replicate(B, {
  s <- sample(x,N,replace=TRUE)
  interval <- c(mean(s)-qt(0.975,N-1)*sd(s)/sqrt(N), mean(s)+qt(0.975,N-1)*sd(s)/sqrt(N))
  between(mu,interval[1], interval[2])
})

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
mean(res)
