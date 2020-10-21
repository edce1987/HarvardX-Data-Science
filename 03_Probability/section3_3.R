beads <- rep(c("red","blue"), times = c(2,3))
X <- ifelse(sample(beads, 1) == "blue", 1, 0)
X

########
color <- rep(c("Black", "Red", "Green"), c(18,18,2))

n <- 1000
X <- sample(ifelse(color=="Red", -1, 1), n, replace=TRUE)

X <- sample(c(-1,1), n, replace=TRUE, prob=c(9/19, 10/19))
length(X[X==1])
S <- sum(X)
S

###
a <- 0
n <- 1000
B <- 10000
S <- replicate(B, {
  X <- sample(c(-1,1), n, replace=TRUE, prob=c(9/19, 10/19))
  sum(X)
}
)

library(tidyverse)
mean(S<a)
avg <- mean(S)
std <- sd(S)

s <- seq(min(S), max(S), length=100)
normal_density <- data.frame(s=s, f=dnorm(s, avg, std))
data.frame(S=S) %>% ggplot(aes(S, ..density..)) +
  geom_histogram(color="black", binwidth=10) +
  ylab("Probability") +
  geom_line(data=normal_density, mapping=aes(s,f), color="blue")

#Key points
#A random variable  𝑋  has a probability distribution function  𝐹(𝑎)  that defines  Pr(𝑋≤𝑎)  over all values of  𝑎 .
#Any list of numbers has a distribution. The probability distribution function of a random variable is defined mathematically and does not depend on a list of numbers.
#The results of a Monte Carlo simulation with a large enough number of observations will approximate the probability distribution of  𝑋 .
#If a random variable is defined as draws from an urn:
#  The probability distribution function of the random variable is defined as the distribution of the list of values in the urn.
#The expected value of the random variable is the average of values in the urn.
#The standard error of one draw of the random variable is the standard deviation of the values of the urn.

#Key points
#Capital letters denote random variables ( 𝑋 ) and lowercase letters denote observed values ( 𝑥 ).
#In the notation  Pr(𝑋=𝑥) , we are asking how frequently the random variable  𝑋  is equal to the value  𝑥 . For example, if  𝑥=6 , this statement becomes  Pr(𝑋=6) .

#CLT

B <- 10^6
X <- sample(c(-1,1), B,replace = TRUE, prob=c(9/19,10/19))
mean(X)

2 * sqrt(90)/19

n <- 1000
sqrt(n) * 2 * sqrt(90)/19

n*(20-18)/38
sqrt(n) *2*sqrt(90)/19
mean(S)
sd(S)

mu <- n * (20-18)/38
se <- sqrt(n) * 2 * sqrt(90)/19
pnorm(0,mu,se)

##Theory
#Key points
#The Central Limit Theorem (CLT) says that the distribution of the sum of a random variable is approximated by a normal distribution.

#The expected value of a random variable,  E[𝑋]=𝜇 , is the average of the values in the urn. This represents the expectation of one draw. 
#The standard error of one draw of a random variable is the standard deviation of the values in the urn.
#The expected value of the sum of draws is the number of draws times the expected value of the random variable. 
#The standard error of the sum of independent draws of a random variable is the square root of the number of draws times the standard deviation of the urn. 
#Equations
#These equations apply to the case where there are only two outcomes,  𝑎  and  𝑏  with proportions  𝑝  and  1−𝑝  respectively. The general principles above also apply to random variables with more than two outcomes.

#Expected value of a random variable: 
#  𝑎*𝑝+𝑏(1−𝑝) 
#Expected value of the sum of n draws of a random variable: 
#  𝑛*(𝑎*𝑝+𝑏(1−𝑝)) 
#Standard deviation of an urn with two values: 
#  abs(𝑏–𝑎)*sqrt(𝑝(1−𝑝)) 
#
#Standard error of the sum of n draws of a random variable:
#  sqrt(𝑛)*abs(𝑏–𝑎)sqrt(𝑝(1−𝑝))

##Test
# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define the number of bets using the variable 'n'
n <- 1000

# Create a vector called 'X' that contains the outcomes of 1000 samples
X <- sample(c(17,-1), n, replace=TRUE, prob=c(p_green,p_not_green))

# Assign the sum of all 1000 outcomes to the variable 'S'
S <- sum(X)

# Print the value of 'S' to the console
S

####Theory

#Key points
#Random variable times a constant
#The expected value of a random variable multiplied by a constant is that constant times its original expected value:
  
#  E[𝑎𝑋]=𝑎𝜇 
#The standard error of a random variable multiplied by a constant is that constant times its original standard error:
  
#  SE[𝑎𝑋]=𝑎𝜎 
#Average of multiple draws of a random variable
#The expected value of the average of multiple draws from an urn is the expected value of the urn ( 𝜇 ).

#The standard deviation of the average of multiple draws from an urn is the standard deviation of the urn divided by the square root of the number of draws ( 𝜎/𝑛√ ).

#The sum of multiple draws of a random variable
#The expected value of the sum of  𝑛  draws of a random variable is  𝑛  times its original expected value:
  
#  E[𝑛𝑋]=𝑛𝜇 
#The standard error of the sum of  𝑛  draws of random variable is  𝑛√  times its original standard error:
  
#  SE[𝑛𝑋]=𝑛√𝜎 

#The sum of multiple different random variables
#The expected value of the sum of different random variables is the sum of the individual expected values for each random variable:
  
#  E[𝑋1+𝑋2+⋯+𝑋𝑛]=𝜇1+𝜇2+⋯+𝜇𝑛 
#The standard error of the sum of different random variables is the square root of the sum of squares of the individual standard errors:
  
#  SE[𝑋1+𝑋2+⋯+𝑋𝑛]=𝜎21+𝜎22+⋯+𝜎2𝑛‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾√ 
#Transformation of random variables
#If  𝑋  is a normally distributed random variable and  𝑎  and  𝑏  are non-random constants, then  𝑎𝑋+𝑏  is also a normally distributed random variable.

########Test

## Make sure you fully follow instructions, including printing values to the console and correctly running the `replicate` loop. If not, you may encounter "Session Expired" errors.

# The variable `n` specifies the number of independent bets on green
n <- 10000

# The variable `B` specifies the number of times we want the simulation to run
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random number generation
set.seed(1)

# Generate a vector `S` that contains the the average outcomes of 10,000 bets modeled 10,000 times
S <- replicate(B, {
  outcome <- sample(c(17,-1), n, replace=TRUE, prob=c(p_green, p_not_green))
  mean(outcome)
})
# Compute the average of `S`
mean(S)

# Compute the standard deviation of `S`
sd(S)

#####Test
options(digits=4)
set.seed(21, sample.kind = "Rounding")
mu <- 1*0.2 + (-0.25)*0.8
44*mu
mu
se <- abs(-0.25-1)*sqrt(0.2*0.8)
sqrt(44)*se
se
1 - pnorm(8, 44*mu,sqrt(44)*se)

B <- 10000
outcome <- replicate(B, {
  res <- sample(c(1,-0.25), 44, replace=TRUE, prob=c(0.2,0.8))
  sum(res)
})
mean(outcome>=8)
####
(0.25*1+0*0.75)*44

p <- seq(0.25, 0.95, 0.05)
testMu <- function(p){
  result <- (p*1+0*(1-p))*44
  result
}

testSe <- function(p){
  result <- abs(0-1)*sqrt(p*(1-p))*sqrt(44)
  result
}
test(p)
sapply(p,test)

mu <- p*1+0*(1-p)
se <- abs(0-1)*sqrt(p-(1-p))

a <- 1-pnorm(35,testMu(p),testSe(p))
a[a>=0.80]

####
p <- 5/38
mu <- 6*p+(-1)*(1-p)
se <- abs(-1-6)*sqrt(p*(1-p))

avgSe500 <- se/sqrt(500)

mu500 <- 500 * -0.0789

se500 <- se*sqrt(500)

pnorm(0, mu500, se500)

########

