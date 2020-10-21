#Course overview
#In this course, we will learn:
  
#  statistical inference, the process of deducing characteristics of a population using data from a random sample
#the statistical concepts necessary to define estimates and margins of errors
#how to forecast future results and estimate the precision of our forecast
#how to calculate and interpret confidence intervals and p-values

#Key points
#Information gathered from a small random sample can be used to infer characteristics of the entire population.
#Opinion polls are useful when asking everyone in the population is impossible.
#A common use for opinion polls is determining voter preferences in political elections for the purposes of forecasting election results.
#The spread of a poll is the estimated difference between support two candidates or options.

library(tidyverse)
library(dslabs)
ds_theme_set()
take_poll(25)    # draw 25 beads

####Theory

#Key points
#When interpreting values of  𝑋, it is important to remember that  𝑋¯  is a random variable with an expected value and standard error that represents the sample proportion of positive events.
#The expected value of  𝑋¯  is the parameter of interest  𝑝 . This follows from the fact that  𝑋¯  is the sum of independent draws of a random variable times a constant  1/𝑁 .
#E(𝑋)=𝑝 
#As the number of draws  𝑁  increases, the standard error of our estimate  𝑋¯  decreases. The standard error of the average of  𝑋¯  over  𝑁  draws is:
#  SE(𝑋)=sqrt(𝑝(1−𝑝)/𝑁) 
#In theory, we can get more accurate estimates of  𝑝  by increasing  𝑁 . In practice, there are limits on the size of  𝑁  due to costs, as well as other factors we discuss later.
#We can also use other random variable equations to determine the expected value of the sum of draws  E(𝑆)  and standard error of the sum of draws  SE(𝑆) .
#E(𝑆)=𝑁*𝑝 
#SE(𝑆)=sqrt(𝑁𝑝(1−𝑝))

p <- seq(0,1,length=100)
N <- c(25,100,1000)
for(i in p){
  se25 <- sqrt(p*(1-p)/25)
  se100 <- sqrt(p*(1-p)/100)
  se1000 <- sqrt(p*(1-p)/1000)
}

N <- c(25,100,1000)
p <- seq(0,1,length=100)

for(n in N){
  for(i in p){
    se <- sqrt(p*(1-p)/N)
    plot(p, se, ylim=c(0,0.1))
  }
}

for(n in N){
  se <- sqrt(p*(1-p)/N)
  plot(p,se)
}

#Expected value of spread
p-(1-p)
#Standard error of spread
2*sqrt(p*(1-p)/N)

#####

X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/25) #SE Estimate
se

pnorm(0.01/se) - pnorm(-0.01/se)

####Theory Central Limit Theorem
#Because  𝑋dash  is the sum of random draws divided by a constant, the distribution of  𝑋dash  is approximately normal.
#We can convert  𝑋dash  to a standard normal random variable  𝑍 : 
#  𝑍=𝑋dash−E(𝑋dash)/SE(𝑋dash) 
#The probability that  𝑋dash  is within .01 of the actual value of  𝑝  is:
#  Pr(𝑍≤.01/sqrt(𝑝(1−𝑝)/𝑁))−Pr(𝑍≤−.01/sqrt(𝑝(1−𝑝)/𝑁)) 
#The Central Limit Theorem (CLT) still works if  𝑋dash  is used in place of  𝑝 . This is called a plug-in estimate. Hats over values denote estimates. Therefore:
#  SEhat(𝑋dash)=sqrt(𝑋dash(1−𝑋dash)/𝑁) 
#Using the CLT, the probability that  𝑋¯  is within .01 of the actual value of  𝑝  is:
#  Pr(𝑍≤.01/sqrt(𝑋dash(1−𝑋dash)/𝑁)−Pr(𝑍≤−.01/sqrt(𝑋dash(1−𝑋¯)/𝑁)

####MoE

2*se #Margin of error of Xdash estimtate
pnorm(2)-pnorm(-2) #MoE

####Monte Carlo Simulation for CLT

B <- 10000
N <- 1000
p <- p_est
X_hat <- replicate(B, {
  X <- sample(c(0,1), size=N, prob=c(1-p,p), replace=TRUE)
  mean(X)
})

p_est <- sample(c(0,1), size=N, prob=c(1-p,p), replace=TRUE)
p_est <- mean(X)

mean(X_hat)
sd(X_hat)

library(gridExtra)
library(tidyverse)

p1 <- data.frame(X_hat=X_hat) %>% ggplot(aes(X_hat)) +
  geom_histogram(binwidth=0.005,color="black")
p2 <- data.frame(X_hat=X_hat) %>% ggplot(aes(sample=X_hat)) +
  stat_qq(dparams=list(mean=mean(X_hat), sd=sd(X_hat))) +
  geom_abline() + ylab("X_hat") + xlab("Theoretical normal")
grid.arrange(p1, p2, nrow=1)

####Theory Spread
#The spread between two outcomes with probabilities  𝑝  and  1−𝑝  is  2𝑝−1 .
#The expected value of the spread is  2𝑋¯−1 .
#The standard error of the spread is  2SE*(𝑋¯) .
#The margin of error of the spread is 2 times the margin of error of  𝑋¯ .

####

N <- 100000
p <- seq(0.35, 0.65, length=100)
SE <- sapply(p,function(x) 2*sqrt(x*(1-x)/N))
data.frame(p=p, SE = SE) %>% ggplot(aes(p,SE)) + geom_line()

#####

#E(X)=X
#SE(X)=sqrt(X*(1-X)/N)   #estimate

##Datacamp Assessment
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# We generated `errors` by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))

# Calculate the standard deviation of `errors`
sqrt(mean(errors^2))

####

# Define `p` as the expected value equal to 0.45
p <- 0.45

# Define `N` as the sample size
N <- 100

# Calculate the standard error
sqrt(p*(1-p)/N)

####

# Define `p` as a proportion of Democratic voters to simulate
p <- 0.45

# Define `N` as the sample size
N <- 100

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `X` as a random sample of `N` voters with a probability of picking a Democrat ('1') equal to `p`
X <- sample(c(1,0), N, prob=c(p,1-p), replace=TRUE)

# Define `X_bar` as the average sampled proportion
X_bar <- mean(X)

# Calculate the standard error of the estimate. Print the result to the console.
sqrt(X_bar*(1-X_bar)/N)

####

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# Calculate the probability that the estimated proportion of Democrats in the population is greater than 0.5. Print this value to the console.
1 - pnorm(0.5, p, sqrt(p*(1-p)/N))

####Important

# Define `N` as the number of people polled
N <-100

# Define `X_hat` as the sample average
X_hat <- 0.51

# Define `se_hat` as the standard error of the sample average
se_hat <- sqrt(X_hat*(1-X_hat)/N)

# Calculate the probability that the error is 0.01 or larger
1 - pnorm(0.01, 0, se_hat) + pnorm(-0.01, 0, se_hat)

####


