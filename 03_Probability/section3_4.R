n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample(c(0,1), n, replace=TRUE, prob=c(1-p,p))
sum(defaults*loss_per_foreclosure)

B <- 10000
losses <- replicate(B, {
  defaults <- sample(c(0,1), n, replace=TRUE, prob=c(1-p,p))
  sum(defaults * loss_per_foreclosure)
})

options(scipen = 999)
options(digits=4)
library(tidyverse)

data.frame(losses_in_millions = losses/10^6) %>% 
  ggplot(aes(losses_in_millions)) + 
  geom_histogram(binwidth = 0.6, col="black")

n*(p*loss_per_foreclosure + (1-p)*0)
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))

#lp+x*(1-p)=0 -> expected value of S <=> x=-lp/(1-p)
loss_per_foreclosure*p/(1-p)
x <- -loss_per_foreclosure*p/(1-p)
x/180000

####
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*(n*p-z*sqrt(n*p*(1-p)))/(n*(1-p) + z*sqrt(n*p*(1-p)))
x

loss_per_foreclosure*p+x*(1-p)

#MC
B <- 10000
profit <- replicate(B, {
  draws <- sample(c(x, loss_per_foreclosure), n, replace=TRUE, prob=c(1-p,p))
  sum(draws)
})
mean(profit)
mean(profit<0)

#Theory
#Equations: Calculating interest rate for 1% probability of losing money
#We want to calculate the value of  𝑥  for which  Pr(𝑆<0)=0.01 . The expected value  E[𝑆]  of the sum of  𝑛=1000  loans given our definitions of  𝑥 ,  𝑙  and  𝑝  is:
  
#  𝜇_s=(𝑙*𝑝+𝑥*(1−𝑝))∗𝑛 
#And the standard error of the sum of  𝑛  loans,  SE[𝑆] , is:
  
#  𝜎_𝑆=∣𝑥−𝑙∣*sqrt(𝑛*𝑝(1−𝑝)) 
#Because we know the definition of a Z-score is  𝑍=𝑥−𝜇𝜎 , we know that  Pr(𝑆<0)=Pr(𝑍<−𝜇𝜎) . Thus,  Pr(𝑆<0)=0.01  equals:
  
#  Pr(𝑍<−{𝑙*𝑝+𝑥*(1−𝑝)}*𝑛/(𝑥−𝑙)*sqrt(𝑛*𝑝(1−𝑝))=0.01 
#z<-qnorm(0.01) gives us the value of  𝑧  for which  Pr(𝑍≤𝑧)=0.01 , meaning:
  
#  𝑧=−{𝑙*𝑝+𝑥*(1−𝑝)}*𝑛/(𝑥−𝑙)*sqrt(𝑛*𝑝*(1−𝑝)) 
#Solving for  𝑥  gives:
  
#  𝑥=−𝑙*𝑛*𝑝−𝑧*sqrt(𝑛*𝑝(1−𝑝))/𝑛*(1−𝑝)+𝑧*sqrt(𝑛*𝑝*(1−𝑝))

####
p <- 0.04
r <- 0.05
loss_per_foreclosure <- -200000
x <- r*180000
loss_per_foreclosure*p+x*(1-p)

z <- qnorm(0.01)
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p+x*(1-p))^2)
n

z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n    # number of loans required

n*(loss_per_foreclosure*p + x * (1-p))    # expected profit over n loans

#MC
B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)

##With dependance
p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length=100), 1)
  draws <- sample(c(x,loss_per_foreclosure), n, prob=c(1-new_p, new_p), replace=TRUE)
  sum(draws)
})
mean(profit)
mean(profit<0)
mean(profit<10000000)

##Final test

library(tidyverse)
library(dslabs)
data("death_prob")
head(death_prob)

p <- death_prob %>% filter(sex=="Female" & age==50) %>% pull(prob)
l <- -150000
g <- 1150
mu <- p*l+g*(1-p)
mu

se <- abs(l-g)*sqrt(p*(1-p))
se

mu1000 <- 1000*mu
se1000 <- sqrt(1000)*se

#CLT
pnorm(0,mu1000,se1000)

pmale <- death_prob %>% filter(sex=="Male" & age==50) %>% pull(prob)

b <- -l*pmale/(1-pmale)
## 𝑛*(𝑎*𝑝+𝑏(1−𝑝)) = profit
1000*(-150000*pmale+b*(1-pmale))=700000
b <- (700000/1000-(-150000*pmale))/(1-pmale)

new_p <- 1459.265
se1000 <- sqrt(1000)*abs(l-new_p)*sqrt(pmale*(1-pmale))
mu1000 <- 1000*(l*pmale+new_p*(1-pmale))

#CLT central limit theorem
pnorm(0, mu1000,se1000)

####Final test 2
options(digits=4)

p_death <- 0.015
l <- -150000
g <- 1150
n <- 1000

mu <- (l*p_death+g*(1-p_death))
mu1000 <- n*mu

se <- abs(l-g)*sqrt(p_death*(1-p_death))
se1000 <- sqrt(n)*se

pnorm(0,mu1000,se1000)

pnorm(-1000000,mu1000,se1000)



fct <- function(p){
  muP <- 1000*((l*p+g*(1-p)))
  seP <- sqrt(1000)*(abs(l-g)*sqrt(p*(1-p)))
  loseP <- pnorm(0,muP,seP)
  loseP
}

p <- seq(0.01, 0.03, 0.001)

frame <- sapply(p, fct)

pf <- data.frame(prob=p)

df <- data.frame(prob=frame,id=pf)

###
p <- seq(0.01,0.03,0.0025)

fct <- function(p){
  muP <- 1000*((l*p+g*(1-p)))
  seP <- sqrt(1000)*(abs(l-g)*sqrt(p*(1-p)))
  loseP <- pnorm(-1000000,muP,seP)
  loseP
}
fct(0.015)

frame2 <- sapply(p,fct)
pf2 <- data.frame(prob=p)
df2 <- data.frame(prob=frame2, id=pf2)
frame2

####

n <- 1000
p_loss <- 0.015
l <- -150000
g <- 1150

set.seed(25, sample.kind = "Rounding")

result <- sample(c(l,g), n, prob=c(p_loss, (1-p_loss)), replace=TRUE)

sum(result)/10^6

set.seed(27, sample.kind = "Rounding")

B <- 10000
sim <- replicate(B, {
  result <- sample(c(l,g), n, prob=c(p_loss, (1-p_loss)), replace=TRUE)
  sum(result)
})
mean(sim< -1000000)

########

p <- 0.015
z <- qnorm(0.05)
n <- 1000
l <- -150000

x <- -l*(n*p-z*sqrt(n*p*(1-p)))/(n*(1-p) + z*sqrt(n*p*(1-p)))

muX <- p*l+x*(1-p)
muX1000 <- 1000*muX

set.seed(28, sample.kind = "Rounding")

B <- 10000
n <- 1000
l <- -150000
p <- 0.015

simu<- replicate(B, {
  result <- result <- sample(c(l,x), n, prob=c(p, (1-p)), replace=TRUE)
  sum(result)
})
mean(simu<0)

####

set.seed(29, sample.kind = "Rounding")

B <- 10000
n <- 1000
l <- -150000
p <- 0.015

simulation <- replicate(B, {
  p_rand <- sample(seq(-0.01, 0.01, length=100),1)
  p_new <- 0.015 + p_rand
  result <- sample(c(l,x), n, prob=c(p_new, (1-p_new)), replace=TRUE)
  sum(result)
})

n <- 1000
B <- 10000
p <- 0.015
x <- 3268
l <- -150000
profit <- replicate(B, {
  new_p <- p + sample(seq(-0.01, 0.01, length=100), 1)
  win <- sample(c(x,l), n, prob=c(1-new_p, new_p), replace=TRUE)
  sum(win)
})
mean(profit)
mean(profit<0)
mean(profit<10000000)

n <- 1000
B <- 10000
l <- -150000
p <- 0.015
x <- 3268
X <- replicate(B, {
  new_p <- p + sample(seq(-0.01, 0.01, length=100), 1)
  Y <- sample(c(x, l), n, replace=TRUE, prob=c(1-new_p, new_p))
  sum(Y)
})

mean(X)

set.seed(29, sample.kind = "Rounding")
n <- 1000
B <- 10000
l <- -150000
p <- 0.015
x <- 3268
X <- replicate(B, {
  new_p <- p + sample(seq(-0.01, 0.01, length=100), 1)
  Y <- sample(c(x, l), n, replace=TRUE, prob=c(1-new_p, new_p))
  sum(Y)
})

mean(X)

mean(X<0)

mean(X< -1000000)
