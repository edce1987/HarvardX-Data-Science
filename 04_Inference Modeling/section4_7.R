#Key points
#We learn how to determine the probability that an observation is due to random variability given categorical, binary or ordinal data.
#Fisher's exact test determines the p-value as the probability of observing an outcome as extreme or more extreme than the observed outcome given the null distribution.
#Data from a binary experiment are often summarized in two-by-two tables.
#The p-value can be calculated from a two-by-two table using Fisher's exact test with the function fisher.test().

# load and inspect research funding rates object
library(tidyverse)
library(dslabs)
data(research_funding_rates)
research_funding_rates

# compute totals that were successful or not successful
totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)

# compare percentage of men/women with awards
totals %>% summarize(percent_men = yes_men/(yes_men + no_men),
                     percent_women = yes_women/(yes_women + no_women))

tab <- matrix(c(3,1,1,3), 2, 2)
rownames(tab) <- c("Poured Before", "Poured After")
colnames(tab) <- c("Guessed Before", "Guessed After")
tab

# p-value calculation with Fisher's Exact Test
fisher.test(tab, alternative = "greater")

####Chi squared
#Key points
#If the sums of the rows and the sums of the columns in the two-by-two table are fixed, then the hypergeometric distribution and  Fisher's exact test can be used. Otherwise, we must use the chi-squared test.
#The chi-squared test compares the observed two-by-two table to the two-by-two table expected by the null hypothesis and asks how likely it is that we see a deviation as large as observed or larger by chance.
#The function chisq.test() takes a two-by-two table and returns the p-value from the chi-squared test.
#The odds ratio states how many times larger the odds of an outcome are for one group relative to another group.
#A small p-value does not imply a large odds ratio. If a finding has a small p-value but also a small odds ratio, it may not be a practically significant or scientifically significant finding. 
#Because the odds ratio is a ratio of ratios, there is no simple way to use the Central Limit Theorem to compute confidence intervals. There are advanced methods for computing confidence intervals for odds ratios that we do not discuss here.

# compute overall funding rate
funding_rate <- totals %>%
  summarize(percent_total = (yes_men + yes_women) / (yes_men + no_men + yes_women + no_women)) %>%
  .$percent_total
funding_rate

# construct two-by-two table for observed data
two_by_two <- tibble(awarded = c("no", "yes"),
                     men = c(totals$no_men, totals$yes_men),
                     women = c(totals$no_women, totals$yes_women))
two_by_two

# compute null hypothesis two-by-two table
tibble(awarded = c("no", "yes"),
       men = (totals$no_men + totals$yes_men) * c(1-funding_rate, funding_rate),
       women = (totals$no_women + totals$yes_women) * c(1-funding_rate, funding_rate))

# chi-squared test
chisq_test <- two_by_two %>%
  select(-awarded) %>% chisq.test()
chisq_test$p.value

# odds of getting funding for men
odds_men <- (two_by_two$men[2] / sum(two_by_two$men)) /
  (two_by_two$men[1] / sum(two_by_two$men))

# odds of getting funding for women
odds_women <- (two_by_two$women[2] / sum(two_by_two$women)) /
  (two_by_two$women[1] / sum(two_by_two$women))

# odds ratio - how many times larger odds are for men than women
odds_men/odds_women

# multiplying all observations by 10 decreases p-value without changing odds ratio
two_by_two %>%
  select(-awarded) %>%
  mutate(men = men*10, women = women*10) %>%
  chisq.test()

####

# The 'errors' data have already been loaded. Examine them using the `head` function.
head(errors)
# Generate an object called 'totals' that contains the numbers of good and bad predictions for polls rated A- and C-
totals <- errors %>% filter(grade %in% c("A-", "C-")) %>% group_by(grade,hit) %>% summarize(num = n()) %>% spread(grade, num)
# Print the proportion of hits for grade A- polls to the console
totals[[2,3]]/sum(totals[[3]])
# Print the proportion of hits for grade C- polls to the console
totals[[2,2]]/sum(totals[[2]])

####

# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)
# Perform a chi-squared test on the hit data. Save the results as an object called 'chisq_test'.
chisq.test <- totals %>% select(-hit) %>% chisq.test()

# Print the p-value of the chi-squared test to the console
chisq.test$p.value

####

# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)

# Generate a variable called `odds_C` that contains the odds of getting the prediction right for grade C- polls
odds_C <- (totals$"C-"[2] / sum(totals$"C-")) / (totals$"C-"[1] / sum(totals$"C-"))

# Generate a variable called `odds_A` that contains the odds of getting the prediction right for grade A- polls
odds_A <- (totals$"A-"[2] / sum(totals$"A-")) / (totals$"A-"[1] / sum(totals$"A-"))

# Calculate the odds ratio to determine how many times larger the odds ratio is for grade A- polls than grade C- polls
odds_A/odds_C

####Test1

library(dslabs)

# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)
#1
p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread
N <- 1500

p #expected value
p*N
se <- sqrt(p*(1-p)/N) #standard error of expected value
se*N
se
p-(1-p) #expected value of spread
2*sqrt(p*(1-p)/N) #SE of spread

#2
head(brexit_polls)
brexit_polls <- brexit_polls %>% mutate(x_hat = (spread+1)/2)

brexit_polls %>% summarize(avg_obs_spread=mean(spread), sd_obs_spread=sd(spread), avg_est=mean(x_hat), sd_est=sd(x_hat))

#3
brexit_polls[1,]
X_hat <- 0.52
N <- 4772
se_hat <- sqrt(X_hat*(1-X_hat)/N)
se_hat
interval <- c(X_hat-qnorm(0.975)*se_hat, X_hat+qnorm(0.975)*se_hat)
interval

####Test2
# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>% mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481
d <- -0.038
p-(1-p) #Exp value of spread
sqrt(p*(1-p)/N) #Exp se of spread
#4

june_polls <- brexit_polls %>% 
  filter(enddate>="2016-06-01") %>%
  mutate(se_x_hat=sqrt(x_hat*(1-x_hat)/samplesize), est_se_sp=2*se_x_hat, lower=spread - qnorm(0.975)*est_se_sp, upper=spread + qnorm(0.975)*est_se_sp, hit=ifelse(d>=lower & d <upper, TRUE,FALSE))

prop <- june_polls %>% filter(lower>0)
20/32
4/32
mean(june_polls$hit)

#5
june_polls %>% group_by(pollster) %>% summarize(prop_hits=mean(hit), N=n()) %>% arrange(prop_hits)

#6
june_polls %>% filter(spread>=0) %>% ggplot(aes(spread, color=poll_type, fill=poll_type)) + geom_histogram(binwidth = 0.05)
june_polls %>% filter(spread>=0) %>% ggplot(aes(spread, color=poll_type, fill=poll_type)) + geom_boxplot()
june_polls %>% group_by(poll_type) %>% summarize(mean=mean(spread))

#7&8
combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)

res <- combined_by_type %>% mutate(se_p_hat=2*(sqrt(p_hat*(1-p_hat)/N)), low=spread-qnorm(0.975)*se_p_hat, up=spread+qnorm(0.975)*se_p_hat, diff=up-low)

##Test 3
# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481


brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>% select(poll_type, hit)

#9

two_by_two <- data.frame(correct=c("no","yes"), 
                 online = a,
                telephone = b)

a <- c(sum(brexit_hit$hit==FALSE & brexit_hit$poll_type=="Online"), sum(brexit_hit$hit==TRUE & brexit_hit$poll_type=="Online"))
b <- c(sum(brexit_hit$hit==FALSE & brexit_hit$poll_type=="Telephone"),sum(brexit_hit$hit==TRUE & brexit_hit$poll_type=="Telephone"))

# chi-squared test
chisq_test <- two_by_two %>%
  select(-correct) %>% chisq.test()
chisq_test$p.value

#10
two_by_two
# odds of getting funding for online
odds_online <- (two_by_two$online[2] / sum(two_by_two$online)) /
  (two_by_two$online[1] / sum(two_by_two$online))

# odds of getting funding for telephone
odds_tele <- (two_by_two$telephone[2] / sum(two_by_two$telephone)) /
  (two_by_two$telephone[1] / sum(two_by_two$telephone))

# odds ratio - how many times larger odds are for online than telephone
odds_online/odds_tele

#11
brexit_polls %>% ggplot(aes(enddate, spread, color=poll_type)) + geom_smooth(method="loess",span=0.4) + geom_point()

#12
brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

brexit_long %>% ggplot(aes(enddate, proportion, color=vote)) + geom_smooth(method="loess", span=0.3)
