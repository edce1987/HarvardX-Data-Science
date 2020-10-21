library(tidyverse)
library(dslabs)
data(heights)

s <- heights %>% 
  filter(sex=="Male") %>% 
  summarize(average=mean(height), standard_deviation=sd(height))

s$average
s$standard_deviation

s <- heights %>% 
  filter(sex=="Male") %>% 
  summarize(median=median(height), minimum=min(height), maximum=max(height))

quantile(heights$height[heights$sex=="Female"])

###################

data(murders)

murders <- murders %>% mutate(murder_rate = total/population*100000)
summarize(murders, mean(murder_rate))

us_murder_rate <- murders %>% summarize(rate=sum(total)/sum(population)*100000) %>% .$rate

class(us_murder_rate)

us_murder_rate %>% .$rate

us_murder_rate$rate

################

heights %>% group_by(sex) %>% summarize(average=mean(height), standard_deviation=sd(height))

murders %>% group_by(region) %>% summarize(median_rate=median(murder_rate))

################

murders %>% arrange(population) %>% head()

murders %>% arrange(region, murder_rate) %>% head()

murders %>% top_n(10, murder_rate)  

murders %>% arrange(desc(murder_rate)) %>% top_n(10)

###
install.packages("NHANES")
library(NHANES)
data(NHANES)

library(dslabs)
data(na_example)
mean(na_example)
sd(na_example)

mean(na_example, na.rm = TRUE)
sd(na_example, na.rm = TRUE)

names(NHANES)
str(NHANES)

NHANES %>% filter(Gender=="Male") %>% 
  group_by(AgeDecade) %>% 
  summarize(average=mean(BPSysAve,na.rm=TRUE), standard_deviation=sd(BPSysAve, na.rm=TRUE))
