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

####
#Key points
#Correlation is not always a good summary of the relationship between two variables.
#The general idea of conditional expectation is that we stratify a population into groups and compute summaries in each group.
#A practical way to improve the estimates of the conditional expectations is to define strata of with similar values of x.
#If there is perfect correlation, the regression line predicts an increase that is the same number of SDs for both variables. If there is 0 correlation, then we don’t use x at all for the prediction and simply predict the average  μy . For values between 0 and 1, the prediction is somewhere in between. If the correlation is negative, we predict a reduction instead of an increase.
library(tidyverse)
library(Lahman)

conditional_avg <- galton_heights %>%
  filter(round(father) == 72) %>%
  summarize(avg = mean(son)) %>% .$avg

# stratify fathers' heights to make a boxplot of son heights
galton_heights %>% mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()

# center of each boxplot
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) +
  geom_point()

# calculate values to plot regression line on original data
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y/s_x #regression coefficient
b <- mu_y - m*mu_x #regression intercept

# add regression line to plot
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)

####

#Key points
#When a pair of random variables are approximated by the bivariate normal distribution, scatterplots look like ovals. They can be thin (high correlation) or circle-shaped (no correlation).
#When two variables follow a bivariate normal distribution, computing the regression line is equivalent to computing conditional expectations.
#We can obtain a much more stable estimate of the conditional expectation by finding the regression line and using it to make predictions.

galton_heights %>%
  mutate(z_father = round((father - mean(father))/sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +
  stat_qq(aes(sample=son)) +
  facet_wrap(~z_father)

####
#Key points
#Conditioning on a random variable X can help to reduce variance of response variable Y.
#The standard deviation of the conditional distribution is  SD(Y∣X=x)=σ_y*sqrt(1−ρ2), which is smaller than the standard deviation without conditioning  σy .
#Because variance is the standard deviation squared, the variance of the conditional distribution is  σ^2_y*(1−ρ^2) .
#In the statement "X explains such and such percent of the variability," the percent value refers to the variance. The variance decreases by  ρ2  percent.
#The “variance explained” statement only makes sense when the data is approximated by a bivariate normal distribution.

#Key point
#There are two different regression lines depending on whether we are taking the expectation of Y given X or taking the expectation of X given Y.

# compute a regression line to predict the son's height from the father's height
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m_1 <-  r * s_y / s_x
b_1 <- mu_y - m_1*mu_x

# compute a regression line to predict the father's height from the son's height
m_2 <-  r * s_x / s_y
b_2 <- mu_x - m_2*mu_y

####Assessment 4
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

#Q8
dat <- female_heights %>% summarize(m_m = mean(mother), s_m=sd(mother), m_d=mean(daughter), s_d=sd(daughter), r=cor(mother,daughter))

#Q9
dat2 <- dat %>% summarize(b=r*(s_d/s_m), a=m_d - m_m*b) 

#Q11
y= 42.5 + 60*0.339
