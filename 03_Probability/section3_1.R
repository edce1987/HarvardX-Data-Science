sample()

beads <- rep(c("red", "blue"), times = c(2,3))

?rep

sample(beads, 1)

#Monte Carlo Simulation
B <- 10000
events <- replicate(B, sample(beads, 1))

tab <- table(events)
tab
prop.table(tab)

#Without replacement / ohne zurücklegen
sample(beads, 5)

#With replacement
events <- sample(beads, B, replace=TRUE)
prop.table(table(events))


#The set.seed() function
#Before we continue, we will briefly explain the following important line of code:
  
#  set.seed(1986) 
#Throughout this book, we use random number generators. This implies that many of the results presented can actually change by chance, which then suggests that a frozen version of the book may show a different result than what you obtain when you try to code as shown in the book. This is actually fine since the results are random and change from time to time. However, if you want to to ensure that results are exactly the same every time you run them, you can set R’s random number generation seed to a specific number. Above we set it to 1986. We want to avoid using the same seed every time. A popular way to pick the seed is the year - month - day. For example, we picked 1986 on December 20, 2018:  2018 − 12 − 20 = 1986.

#You can learn more about setting the seed by looking at the documentation:
  
#  ?set.seed
#In the exercises, we may ask you to set the seed to assure that the results you obtain are exactly what we expect them to be.

#Important note on seeds in R 3.5 and R 3.6
#R was recently updated to version 3.6 in early 2019. In this update, the default method for setting the seed changed. This means that exercises, videos, textbook excerpts and other code you encounter online may yield a different result based on your version of R.

#If you are running R 3.6, you can revert to the original seed setting behavior by adding the argument sample.kind="Rounding". For example:
  
set.seed(1)
set.seed(1, sample.kind="Rounding")    # will make R 3.6 generate a seed as in R 3.5
#Using the sample.kind="Rounding" argument will generate a message:
  
#  non-uniform 'Rounding' sampler used

#This is not a warning or a cause for alarm - it is a confirmation that R is using the alternate seed generation method, and you should expect to receive this message in your console.

#If you use R 3.6, you should always use the second form of set.seed() in this course series (outside of DataCamp assignments). Failure to do so may result in an otherwise correct answer being rejected by the grader. In most cases where a seed is required, you will be reminded of this fact.

#An important application of the mean() function
#In R, applying the mean() function to a logical vector returns the proportion of elements that are TRUE. It is very common to use the mean() function in this way to calculate probabilities and we will do so throughout the course.

#Suppose you have the vector beads from a previous video:
  
beads <- rep(c("red", "blue"), times = c(2,3))
#beads
#[1] "red" "red" "blue" "blue" "blue"
#To find the probability of drawing a blue bead at random, you can run:
  
mean(beads == "blue")
#[1] 0.6
#This code is broken down into steps inside R. First, R evaluates the logical statement beads == "blue", which generates the vector:
  
#  FALSE FALSE TRUE TRUE TRUE
#When the mean function is applied, R coerces the logical values to numeric values, changing TRUE to 1 and FALSE to 0:
  
#  0 0 1 1 1
#The mean of the zeros and ones thus gives the proportion of TRUE values. As we have learned and will continue to see, probabilities are directly related to the proportion of events that satisfy a requirement.

########Independence
options(digits=3)
x <- sample(beads, 5)
x[2:5]

#Assessment 1
3/15
12/15
3/15*12/14
3/15*12/15

########Permutations

number <- "Three"
suit <- "Hearts"
paste(number, suit)

paste(letters[1:5], as.character(1:5))

expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))

#Generate deck of cards

suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number=numbers, suit=suits)
deck <- paste(deck$number, deck$suit)
deck

#Prob for king
kings <- paste("King", suits)
mean(deck %in% kings)

#Permutations
install.packages("gtools")
library(gtools)

permutations(5,2)

#Optionally
all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n,5)
all_phone_numbers[index,]

hands <- permutations(52, 2, v=deck)
hands

first_card <- hands[,1]
second_card <- hands[,2]

sum(first_card %in% kings)

sum(first_card %in% kings & second_card %in% kings) / sum(first_card %in% kings)

mean(first_card %in% kings & second_card %in% kings) / mean(first_card %in% kings)

####
permutations(3,2)
combinations(3,2)

aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number=facecard, suit=suits)
facecard <- paste(facecard$number, facecard$suit)
hands <- combinations(52,2,v=deck)
hands
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

#consider both possibilities
mean((hands[,1] %in% aces & hands[,2] %in% facecard) | (hands[,2] %in% aces & hands[,1] %in% facecard))

#Montecarlo of drawing ace+facecard
hand <- sample(deck,2)
hand

B <- 100000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) |
    (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)

########Birthday problem
n <- 50
bdays <- sample(1:365, n, replace=TRUE)
bdays
duplicated(c(1,2,3,2,3,4,5))
any(duplicated(bdays))

B <- 10000
results <- replicate(B, {
  bdays <- sample(1:365, n, replace=TRUE)
  any(duplicated(bdays))
})
mean(results)

########sapply

compute_prob <- function(n, B=10000){
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace=TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

n <- seq(1,60)
x <- 1:10
sqrt(x)
y <- 1:10
x*y

sapply(x, sqrt)

prob <- sapply(n, compute_prob)
plot(n, prob)

########Compute exact bday prob
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365
  1 - prod(prob_unique)
}

eprob <- sapply(n, exact_prob)
plot(n, eprob)
lines(n, eprob, col="red")

########How many repetitions for Monte carlo

B <- 10^seq(1,5,len=100)
compute_prob <- function(B, n=22){
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace=TRUE)
  })
  mean(same_day)
}

prob <- sapply(B, compute_prob)
plot(log10(B), prob, type="l")

####Datacamp assessment
B <- 10000
celtic_wins <- replicate(B, {
  simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
  any("win" %in% simulated_games)
  })

mean(celtic_wins)

########Monty Hall Sticking to door
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat", "goat"))
  prize_door <- doors[prize=="car"]
  my_pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)
  stick <- my_pick
  stick == prize_door
})
mean(stick)

#Switching the door
switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car", "goat", "goat"))
  prize_door <- doors[prize=="car"]
  my_pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)
  stick <- my_pick
  switch <- doors[!doors %in% c(my_pick, show)]
  switch == prize_door
})
mean(switch)

####
n<-7-1
outcomes <- c(0,1)
l <- rep(list(outcomes), n)
l
possibilities <- expand.grid(l)
possibilities
results <- rowSums(possibilities)
mean(results>=4)

simulated_games <- sample(c(0,1), 6, replace = TRUE, prob = c(0.5, 0.5))
simulated_games
sum(simulated_games)

results <- replicate(B, {
  simulated_games <- sample(c(0,1), 6, replace = TRUE, prob = c(0.5, 0.5))
  sum(simulated_games) > 4
})
mean(results)

#######2 Datacamp assessment
B <- 10000
results <- replicate(B, {
  simulated_games <- sample(c(0,1), 6, replace = TRUE) 
  sum(simulated_games)
})

####Task 3
# Let's assign the variable 'p' as the vector of probabilities that team A will win.
p <- seq(0.5, 0.95, 0.025)

# Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function based on a Monte Carlo simulation:
prob_win <- function(p){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), 7, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=4
  })
  mean(result)
}

# Apply the 'prob_win' function across the vector of probabilities that team A will win to determine the probability that team B will win. Call this object 'Pr'.
Pr <- sapply(p,prob_win)

# Plot the probability 'p' on the x-axis and 'Pr' on the y-axis.
plot(p, Pr)

library(gtools)
library(tidyverse)

####Test 1
a <- permutations(8,3)
b <- permutations(3,3)
b

3/8
3/8 * 2/7 * 1/6

runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
set.seed(1, sample.kind="Rounding") 

B <- 10000
jamaica_wins <- replicate(B, {
  race <- sample(runners, 3, replace = FALSE)
  all(race=="Jamaica")
})

race
jamaica_wins
mean(jamaica_wins)

####Test 2
a <- permutations(6,2)
combinations(6,3)
starter <- c(1,2,3,4,5,6)
side1 <- c(1,2,3,4,5,6)
side2 <- c(1,2,3,4,5,6)
side3 <- c(1,2,3,4,5,6)
drink <- c(1,2,3)
possibilities <- expand.grid(starter=starter, side1=side1, side2=side2, side3=side3, drink=drink)

subset <- possibilities %>% filter(side1 != side2 & side1 != side3 & side2 != side3)
permutations(6,2)
subset
6*15*2
unique(subset)

2160/3
6*20*3
9 * nrow(combinations(6,2)) * 3
n <- 1:12
entree <- function(n){
  comb <- n*(6*5)/2*3
  comb
}
sapply(n, entree)

s <- 2:12
entree <- function(s){
  comb <- 6*nrow(combinations(s,2))*3
  comb
}
sapply(s, entree)

########Test 3
head(esoph)
data(esoph)
library(tidyverse)
all_cases <- sum(esoph$ncases)
all_controls <- sum(esoph$ncontrols)

esoph %>% select(alcgp, ncases) %>% filter(alcgp=="120+") %>% pull(ncases) %>% sum()

45/all_cases
all_cases + all_controls
45/1175
options(digits=3)

subset <- esoph %>% select(alcgp, ncases, ncontrols) %>% filter(alcgp=="120+")
sum(subset$ncontrols)
45/(45+67)

lc <- esoph %>% select(alcgp, ncases) %>% filter(alcgp=="0-39g/day") %>% pull(ncases) %>% sum()
lcc <- esoph %>% select(alcgp, ncontrols) %>% filter(alcgp=="0-39g/day") %>% pull(ncontrols) %>% sum()
lc/(lcc+lc)

pbA <- all_cases/(all_cases+all_controls)

sc <- esoph %>% filter(ncases > 0 & tobgp %in% c("10-19", "20-29", "30+")) %>% select(ncases) %>% sum()

nsc <- esoph %>% filter(ncases > 0 & tobgp %in% c("10-19", "20-29", "30+")) %>% select(ncontrols) %>% sum()

(all_cases/(all_cases+all_controls)) * (sc/(sc+nsc))

(0.17*0.268) / 0.17

sc <- esoph %>% filter(tobgp %in% c("10-19", "20-29", "30+")) %>% select(ncases) %>% sum()

nsc <- esoph %>% filter(tobgp %in% c("10-19", "20-29", "30+")) %>% select(ncontrols) %>% sum()

pbB <- sc/(sc+nsc)

(pbA * pbB)/pbA

# Given that a person is a case, what is the probability that they smoke 10g or
#  more a day?
library(tidyverse)
esoph %>% summarize(tot_cases = sum(ncases))
esoph %>% filter(tobgp != "0-9g/day") %>%
  summarize(smoking10_cases = sum(ncases))
122/200

####
esoph %>% summarize(tot_cases = sum(ncontrols))
esoph %>% filter(tobgp != "0-9g/day") %>%
  summarize(smoking10_cases = sum(ncontrols))
450/975

####
esoph %>% filter(alcgp =="120+") %>% summarize(sum(ncases))
45/200

####
esoph %>% filter(tobgp=="30+") %>% summarize(n=sum(ncases))
31/200                                              

####
esoph %>% filter(tobgp=="30+" | alcgp=="120+") %>% summarize(n=sum(ncases))
66/200

####
esoph %>% filter(tobgp=="30+" & alcgp=="120+") %>% summarize(n=sum(ncases))
10/200

####
esoph %>% filter(alcgp=="120+") %>% summarize(nc=sum(ncontrols))
67/975

####
0.225/0.0687

####
esoph %>% filter(tobgp=="30+") %>% summarize(nc=sum(ncontrols))
82/975

####
esoph %>% filter(tobgp=="30+" & alcgp=="120+") %>% summarize(nc=sum(ncontrols))
13/975

####
esoph %>% filter(tobgp=="30+" | alcgp=="120+") %>% summarize(nc=sum(ncontrols))
136/975

####
0.33/0.139