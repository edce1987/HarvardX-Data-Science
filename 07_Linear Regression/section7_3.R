#Key points
#Association/correlation is not causation.
#p-hacking is a topic of much discussion because it is a problem in scientific publications. Because publishers tend to reward statistically significant results over negative results, there is an incentive to report significant results.

library(tidyverse)

# generate the Monte Carlo simulation
N <- 25
g <- 1000000
sim_data <- tibble(group = rep(1:g, each = N), x = rnorm(N * g), y = rnorm(N * g))

# calculate correlation between X,Y for each group
res <- sim_data %>% 
  group_by(group) %>% 
  summarize(r = cor(x, y)) %>% 
  arrange(desc(r))
res

# plot points from the group with maximum correlation
sim_data %>% filter(group == res$group[which.max(res$r)]) %>%
  ggplot(aes(x, y)) +
  geom_point() + 
  geom_smooth(method = "lm")

# histogram of correlation in Monte Carlo simulations
res %>% ggplot(aes(x=r)) + geom_histogram(binwidth = 0.1, color = "black")

# linear regression on group with maximum correlation
library(broom)
sim_data %>% 
  filter(group == res$group[which.max(res$r)]) %>%
  do(tidy(lm(y ~ x, data = .)))

#Spearman
#Key points
#Correlations can be caused by outliers.
#The Spearman correlation is calculated based on the ranks of data.
# simulate independent X, Y and standardize all except entry 23
set.seed(1985)
x <- rnorm(100,100,1)
y <- rnorm(100,84,1)
x[-23] <- scale(x[-23])
y[-23] <- scale(y[-23])

# plot shows the outlier
qplot(x, y, alpha = 0.5)

# outlier makes it appear there is correlation
cor(x,y)
cor(x[-23], y[-23])

# use rank instead
qplot(rank(x), rank(y))
cor(rank(x), rank(y))

# Spearman correlation with cor function
cor(x, y, method = "spearman")

####
#Key points
#Another way association can be confused with causation is when the cause and effect are reversed.
#As discussed in the video, in the Galton data, when father and son were reversed in the regression, the model was technically correct. The estimates and p-values were obtained correctly as well. What was incorrect was the interpretation of the model.

# cause and effect reversal using son heights to predict father heights
library(HistData)
data("GaltonFamilies")
GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight) %>% 
  do(tidy(lm(father ~ son, data = .)))

####Confounding
#Key points
#If X and Y are correlated, we call Z a confounder if changes in Z causes changes in both X and Y.
# UC-Berkeley admission data
library(dslabs)
data(admissions)
admissions

# percent men and women accepted
admissions %>% group_by(gender) %>% 
  summarize(percentage = 
              round(sum(admitted*applicants)/sum(applicants),1))

# test whether gender and admission are independent
admissions %>% group_by(gender) %>% 
  summarize(total_admitted = round(sum(admitted / 100 * applicants)), 
            not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>% 
  do(tidy(chisq.test(.)))

# percent admissions by major
admissions %>% select(major, gender, admitted) %>%
  spread(gender, admitted) %>%
  mutate(women_minus_men = women - men)

# plot total percent admitted to major versus percent women applicants
admissions %>% 
  group_by(major) %>% 
  summarize(major_selectivity = sum(admitted * applicants) / sum(applicants),
            percent_women_applicants = sum(applicants * (gender=="women")) /
              sum(applicants) * 100) %>%
  ggplot(aes(major_selectivity, percent_women_applicants, label = major)) +
  geom_text()

# plot number of applicants admitted and not
admissions %>%
  mutate(yes = round(admitted/100*applicants), no = applicants - yes) %>%
  select(-applicants, -admitted) %>%
  gather(admission, number_of_students, -c("major", "gender")) %>%
  ggplot(aes(gender, number_of_students, fill = admission)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(. ~ major)

admissions %>% 
  mutate(percent_admitted = admitted * applicants/sum(applicants)) %>%
  ggplot(aes(gender, y = percent_admitted, fill = major)) +
  geom_bar(stat = "identity", position = "stack")

# condition on major and then look at differences
admissions %>% ggplot(aes(major, admitted, col = gender, size = applicants)) + geom_point()

# average difference by major
admissions %>%  group_by(gender) %>% summarize(average = mean(admitted))

#Simpsons Paradox
#Key point
#Simpson’s Paradox happens when we see the sign of the correlation flip when comparing the entire dataset with specific strata. 

##Assessment
library(dslabs)
data("research_funding_rates")
research_funding_rates

#Q1

# construct two-by-two table for observed data
totalMenAwarded <- sum(research_funding_rates$awards_men)
totalMenNotAwarded <- sum(research_funding_rates$applications_men) - sum(research_funding_rates$awards_men)
totalWomenAwarded <- sum(research_funding_rates$awards_women)
totalWomenNotAwarded <- sum(research_funding_rates$applications_women) - sum(research_funding_rates$awards_women)

two_by_two <- tibble(awarded = c("no", "yes"), 
       men = c(totalMenNotAwarded, totalMenAwarded), 
       women = c(totalWomenNotAwarded, totalWomenAwarded))

two_by_two

#Q2
two_by_two %>% mutate(successMen = men[2]/sum(men), successWomen = women[2]/sum(women))

#Q3
# chi-squared test
chisq_test <- two_by_two %>%
  select(-awarded) %>% chisq.test()
chisq_test$p.value

tidy(chisq_test)

#Q4
dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")
dat

dat %>% ggplot(aes(success, discipline, col = gender, size = applications)) + geom_point()
