####Theory
#Key points
#The goal of a baseball game is to score more runs (points) than the other team.
#Each team has 9 batters who have an opportunity to hit a ball with a bat in a predetermined order. 
#Each time a batter has an opportunity to bat, we call it a plate appearance (PA).
#The PA ends with a binary outcome: the batter either makes an out (failure) and returns to the bench or the batter doesn’t (success) and can run around the bases, and potentially score a run (reach all 4 bases).
#We are simplifying a bit, but there are five ways a batter can succeed (not make an out):
#  Bases on balls (BB): the pitcher fails to throw the ball through a predefined area considered to be hittable (the strike zone), so the batter is permitted to go to first base.
#Single: the batter hits the ball and gets to first base.
#Double (2B): the batter hits the ball and gets to second base.
#Triple (3B): the batter hits the ball and gets to third base.
#Home Run (HR): the batter hits the ball and goes all the way home and scores a run.
#Historically, the batting average has been considered the most important offensive statistic. To define this average, we define a hit (H) and an at bat (AB). Singles, doubles, triples and home runs are hits. The fifth way to be successful, a walk (BB), is not a hit. An AB is the number of times you either get a hit or make an out; BBs are excluded. The batting average is simply H/AB and is considered the main measure of a success rate.
#Note: The video states that if you hit AFTER someone who hits many home runs, you will score many runs, while the textbook states that if you hit BEFORE someone who hits many home runs, you will score many runs. The textbook wording is accurate.

##
install.packages("Lahman")
library(Lahman)
install.packages("dslabs")
ds_theme_set()
library(tidyverse)

View(Teams)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR/G, R_per_game = R/G) %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

#Stolen bases per game
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB/G, R_per_game = R/G) %>%
  ggplot(aes(SB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

#Base on ball
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

?Teams

##Assessment1
#Q6
library(Lahman)
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

#Q7
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(W_per_game = W/G, E_per_game = E/G) %>%
  ggplot(aes(W_per_game, E_per_game)) +
  geom_point(alpha = 0.5)

#Q8
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X3_per_game = X3B/G, X2_per_game = X2B/G) %>%
  ggplot(aes(X2_per_game, X3_per_game)) +
  geom_point(alpha = 0.5)

cor(Teams$X3B/Teams$G, Teams$X2B/Teams$G)

####
#Key points
#Galton tried to predict sons' heights based on fathers' heights.
#The mean and standard errors are insufficient for describing an important characteristic of the data: the trend that the taller the father, the taller the son.
#The correlation coefficient is an informative summary of how two variables move together that can be used to predict one variable using the other.

library(tidyverse)
install.packages("HistData")

library(HistData)
data("GaltonFamilies")

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

options(digits = 3)

galton_heights %>% summarize(mean(father), sd(father), mean(son), sd(son))

galton_heights %>% ggplot(aes(father, son)) + geom_point(alpha = 0.5)

##
#Key points
#The correlation coefficient is defined for a list of pairs  (x1,y1),...,(xn,yn)  as the product of the standardized values:  (xi−μxσx)(yi−μyσy) .
#The correlation coefficient essentially conveys how two variables move together.
#The correlation coefficient is always between -1 and 1.

galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)

#Key points
#The correlation that we compute and use as a summary is a random variable.
#When interpreting correlations, it is important to remember that correlations derived from samples are estimates containing uncertainty.
#Because the sample correlation is an average of independent draws, the central limit theorem applies.

set.seed(0)

R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  summarize(cor(father,son))
R

B <- 1000
N <- 25
R <- replicate(B, {
  sample_n(galton_heights, 25, replace = TRUE) %>%
    summarize(r = cor(father,son)) %>% pull(r)
})

data.frame(R) %>% ggplot(aes(R)) + geom_histogram(binwidth =  0.05, color = "black")

# Monte Carlo simulation to show distribution of sample correlation
B <- 1000
N <- 25
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    summarize(r = cor(father, son)) %>%
    pull(r)
})
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))

mean(R)
sd(R)

# QQ-plot to evaluate whether N is large enough
data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))

####Assessment 2
B <- 1000
N <- 25
R <- replicate(B, {
  sample_n(galton_heights, 50, replace = TRUE) %>%
    summarize(r = cor(father,son)) %>% pull(r)
})
mean(R)
sd(R)

#Q7-9

library(Lahman)
Teams %>% filter(yearID %in% 1961:2001) %>%
  summarize(correlation = cor(X2B/G, X3B/G)) %>% pull(correlation)

