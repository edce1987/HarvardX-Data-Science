library(tidyverse)
library(dslabs)
data(heights)

x <- heights %>% filter(sex=="Male") %>% .$height

F <- function(a) mean(x<=a)
#Prob that student is taller than 70inch
1 - F(70)

####
pnorm()

1 - pnorm(70.5, mean(x), sd(x))

plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(X=a)")

#Exact
mean(x<= 68.5) - mean(x <= 67.5)
mean(x<= 69.5) - mean(x <= 68.5)
mean(x<= 70.5) - mean(x <= 69.5)
#Theoretical
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# discretization probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))

####Probability density function
dnorm()

###Theory
#Plotting the probability density for the normal distribution
#We can use dnorm() to plot the density curve for the normal distribution. dnorm(z) gives the probability density  𝑓(𝑧)  of a certain z-score, so we can draw a curve by calculating the density over a range of possible values of z.

#First, we generate a series of z-scores covering the typical range of the normal distribution. Since we know 99.7% of observations will be within  −3≤𝑧≤3 , we can use a value of  𝑧  slightly larger than 3 and this will cover most likely values of the normal distribution. Then, we calculate  𝑓(𝑧) , which is dnorm() of the series of z-scores. Last, we plot  𝑧  against  𝑓(𝑧) .

library(tidyverse)
x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()
#Here is the resulting plot:
  
#  Plot of the normal distribution generated using the dnorm function.

#Note that dnorm() gives densities for the standard normal distribution by default. Probabilities for alternative normal distributions with mean mu and standard deviation sigma can be evaluated with:
  
dnorm(z, mu, sigma)

####
rnorm() #Mimic naturally occuring events

x <- heights %>% filter(sex=="Male") %>% .$height
n <- length(x)
avg <- mean(x)
sd <- sd(x)
simulated_heights <- rnorm(n, avg, sd)
test <- rnorm(800, avg, sd)
sim %>% ggplot(aes(a)) + geom_density()

##MC Simulation
B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, sd)
  max(simulated_data)
})
mean(tallest >= 84)
tallest

####Norm
x <- seq(-4,4,length.out = 100)
data.frame(x, f=dnorm(x)) %>% ggplot(aes(x,f)) + geom_line()

#Student
x <- seq(-4,4,length.out = 100)
data.frame(x, f=dt(x, 5)) %>% ggplot(aes(x,f)) + geom_line()

####Assessment
# The variable `B` specifies the number of times we want the simulation to run.
B <- 1000

# Use the `set.seed` function to make sure your answer matches the expected result after random number generation.
set.seed(1)

# Create an object called `highestIQ` that contains the highest IQ score from each random distribution of 10,000 people.
highestIQ <- replicate(B, {
  smartest <- rnorm(10000, 100, 15)
  max(smartest)
})

# Make a histogram of the highest IQ scores.
hist(highestIQ)

####Assessment 2
set.seed(16)
set.seed(16, sample.kind="Rounding")
options(digits=3)

act_scores <- rnorm(10000, 20.9, 5.7)

avg <- mean(act_scores)
s <- sd(act_scores)
df <- data.frame(score=act_scores) 

df %>% filter(score >= 36) %>% summarize(score) %>% nrow()
nrow(subset(df, score>=36))

1 - pnorm(30, avg, s)

pnorm(10, avg, s)
 x <- 1:36
f_x <- dnorm(x, 20.9, 5.7) 
plot(x, f_x)

####Assessment 3
act_scores_z <- (act_scores - avg)/s 
act_scores_z #Standardized
test <- avg + 2*s
1 - pnorm(2, 0, 1)
qnorm(0.975, avg, s)

fct <- function(a){
  prob <- pnorm(a, avg, s)
  prob
}
fct(20)
cdf <- sapply(1:36, fct) 
plot(1:36, cdf)
cdf
cdf >= 0.95
dfc <- data.frame(int = 1:36, prob = cdf)
dfc %>% filter(prob >= 0.95) %>% first()

qnorm(0.95, 20.9, 5.7)

p <- seq(0.01,0.99,0.01)
sample_quantiles <- quantile(act_scores, p)

theoretical_quantiles <- qnorm(p, 20.9, 5.7)

plot(theoretical_quantiles, sample_quantiles)
