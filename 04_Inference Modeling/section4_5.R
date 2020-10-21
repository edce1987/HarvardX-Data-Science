####Bayesian statistics
#Key points
#Bayes' Theorem states that the probability of event A happening given event B is equal to the probability of both A and B divided by the probability of event B:
#Pr(A???B)=Pr(B???A)*Pr(A)/Pr(B) 
#Bayes' Theorem shows that a test for a very rare disease will have a high percentage of false positives even if the accuracy of the test is high.
#Equations: Cystic fibrosis test probabilities
#In these probabilities, + represents a positive test, - represents a negative test,  D=0  indicates no disease, and  D=1  indicates the disease is present.
#
#Probability of having the disease given a positive test:  Pr(D=1???+) 
#99% test accuracy when disease is present:  Pr(+???D=1)=0.99 
#99% test accuracy when disease is absent:  Pr(??????D=0)=0.99 
#Rate of cystic fibrosis:  Pr(D=1)=0.00025 
#Bayes' theorem can be applied like this:

#Pr(D=1???+)=Pr(+???D=1)???Pr(D=1)Pr(+) 
#Pr(D=1???+)=Pr(+???D=1)???Pr(D=1)Pr(+???D=1)???Pr(D=1)+Pr(+???D=0)???Pr(D=0) 
#Substituting known values, we obtain:

#0.99???0.000250.99???0.00025+0.01???0.99975=0.02

####

prev <- 0.00025
N <- 100000
outcome <- sample(c("Disease","Healthy"), N, prob=c(prev, 1-prev), replace=TRUE)

N_D <- sum(outcome == "Disease")    # number with disease
N_H <- sum(outcome == "Healthy")    # number healthy

# for each person, randomly determine if test is + or -
accuracy <- 0.99
test <- vector("character", N)
test[outcome == "Disease"] <- sample(c("+", "-"), N_D, replace=TRUE, prob = c(accuracy, 1-accuracy))
test[outcome == "Healthy"] <- sample(c("-", "+"), N_H, replace=TRUE, prob = c(accuracy, 1-accuracy))

table(outcome, test)

####Hierarchical models
#Key points
#Hierarchical models use multiple levels of variability to model results. They are hierarchical because values in the lower levels of the model are computed using values from higher levels of the model.
#We model baseball player batting average using a hierarchical model with two levels of variability:
#  p???N(??,??)  describes player-to-player variability in natural ability to hit, which has a mean  ??  and standard deviation  ?? .
#Y???p???N(p,??)  describes a player's observed batting average given their ability  p , which has a mean  p  and standard deviation  ??=p(1???p)/N????????????????????????????????? . This represents variability due to luck.
#In Bayesian hierarchical models, the first level is called the prior distribution and the second level is called the sampling distribution.
#The posterior distribution allows us to compute the probability distribution of  p  given that we have observed data  Y .
#By the continuous version of Bayes' rule, the expected value of the posterior distribution  p  given  Y=y  is a weighted average between the prior mean  ??  and the observed data  Y :
#  E(p???y)=B??+(1???B)Y      where      B=??2??2+??2 
#The standard error of the posterior distribution  SE(p???Y)2  is  11/??2+1/??2 . Note that you will need to take the square root of both sides to solve for the standard error.
#This Bayesian approach is also known as shrinking. When  ??  is large,  B  is close to 1 and our prediction of  p  shrinks towards the mean (\mu). When  ??  is small,  B  is close to 0 and our prediction of  p  is more weighted towards the observed data  Y .

####

# Define `Pr_1` as the probability of the first son dying of SIDS
Pr_1 <- 1/8500

# Define `Pr_2` as the probability of the second son dying of SIDS
Pr_2 <- 1/100

# Define `Pr_B` as the probability of both sons dying of SIDS
Pr_B <- Pr_1*Pr_2

# Define Pr_A as the rate of mothers that are murderers
Pr_A <- 1/1000000

# Define Pr_BA as the probability that two children die without evidence of harm, given that their mother is a murderer
Pr_BA <- 0.50

# Define Pr_AB as the probability that a mother is a murderer, given that her two children died with no evidence of physical harm. Print this value to the console.
Pr_AB <- (Pr_BA*Pr_A)/Pr_B
Pr_AB

####

# Load the libraries and poll data
library(dplyr)
library(dslabs)
data(polls_us_election_2016)

# Create an object `polls` that contains the spread of predictions for each candidate in Florida during the last polling days
polls <- polls_us_election_2016 %>% filter(state == "Florida" & enddate >= "2016-11-04" ) %>% mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Examine the `polls` object using the `head` function
head(polls)

# Create an object called `results` that has two columns containing the average spread (`avg`) and the standard error (`se`). Print the results to the console.
results <- polls %>% summarize(avg=mean(spread), se=sd(spread)/sqrt(n()))
results

####

# The results` object has already been loaded. Examine the values stored: `avg` and `se` of the spread
results

# Define `mu` and `tau`
mu <- 0
tau <- 0.01

# Define a variable called `sigma` that contains the standard error in the object `results`
sigma <- results$se

# Define a variable called `Y` that contains the average in the object `results`
Y <- results$avg

# Define a variable `B` using `sigma` and `tau`. Print this value to the console.
B <- sigma^2 / (sigma^2 + tau^2)
B
# Calculate the expected value of the posterior distribution
B*mu+(1-B)*Y

####

# Here are the variables we have defined in previous exercises
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

# Construct the 95% credible interval. Save the lower and then the upper confidence interval to a variable called `ci`.
est <- B * mu + (1 - B) * Y
est
ci <- c(est - qnorm(0.975) * se, est + qnorm(0.975) * se)
ci

####

# Assign the expected value of the posterior distribution to the variable `exp_value`
exp_value <- B*mu + (1-B)*Y 

# Assign the standard error of the posterior distribution to the variable `se`
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

# Using the `pnorm` function, calculate the probability that the actual spread was less than 0 (in Trump's favor). Print this value to the console.
pnorm(0, exp_value,se)

####

# Define the variables from previous exercises
mu <- 0
sigma <- results$se
Y <- results$avg

# Define a variable `taus` as different values of tau
taus <- seq(0.005, 0.05, len = 100)

# Create a function called `p_calc` that generates `B` and calculates the probability of the spread being less than 0
p_calc <- function(tau){
  B <- sigma^2/(sigma^2+tau^2)
  exp_value <- pnorm(0,B*mu+(1-B)*Y, sqrt( 1/ (1/sigma^2 + 1/tau^2)))
}
# Create a vector called `ps` by applying the function `p_calc` across values in `taus`
ps <- sapply(taus,p_calc)

# Plot `taus` on the x-axis and `ps` on the y-axis
plot(taus,ps)