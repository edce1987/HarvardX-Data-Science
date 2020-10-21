library(dslabs)
library(ggplot2)
library(tidyverse)
library(dplyr)
data(heights)

heights %>% ggplot(aes(sex, height)) + geom_point() + geom_boxplot()

heights %>% ggplot(aes(height)) + geom_histogram()

heights %>% ggplot(aes(height)) + geom_density()

color_blind_friendly_cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p1 <- data.frame(x = 1:8, y = 1:8, col = as.character(1:8)) %>%
  ggplot(aes(x, y, color = col)) +
  geom_point(size = 5)
p1 + scale_color_manual(values = color_blind_friendly_cols)

library(dplyr)
library(ggplot2)
library(dslabs)
data("murders")
murders %>% mutate(rate = total/population*100000) %>%
  group_by(region) %>%
  summarize(avg = mean(rate)) %>%
  mutate(region = factor(region)) %>%
  ggplot(aes(region, avg)) +
  geom_bar(stat="identity") +
  ylab("Murder Rate Average")

########

library(tidyverse)
library(dslabs)
data(gapminder)

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

dat <- gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)

dat %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                           location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy") 

########

data("us_contagious_diseases")
str(us_contagious_diseases)

the_disease <- "Measles"
dat <- us_contagious_diseases %>% 
  filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
  mutate(rate = count / population * 10000) %>%
  mutate(state = reorder(state, rate))

dat %>% filter(state == "California") %>%
  ggplot(aes(year, rate)) + geom_line() + ylab("Cases per 10,000") + geom_vline(xintercept=1963, col="blue")

library(RColorBrewer)
display.brewer.all(type="div")

dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color="grey50") +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_gradientn(colors=brewer.pal(9, "Reds"), trans="sqrt") +
  geom_vline(xintercept=1963, col="blue") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  ggtitle(the_disease) +
  ylab("") + xlab("")

avg <- us_contagious_diseases %>%
  filter(disease==the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)

dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") +
  ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"), color = "black") +
  geom_vline(xintercept = 1963, col = "blue")

options(digits = 3)
library(tidyverse)
install.packages("titanic")
library(titanic)

titanic <- titanic_train %>% 
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>% 
  mutate(Survived = factor(Survived), Pclass = factor(Pclass), Sex = factor(Sex))

str(titanic)

titanic %>% ggplot(aes(Age, group=Sex, color=Sex)) + geom_density()

f40 <- titanic %>% filter(Sex=="female" & Age==40)
m40 <- titanic %>% filter(Sex=="male" & Age==40)                   
prop.table(table(titanic$Sex[titanic$Age<17]))
table(titanic$Sex[titanic$Age<17])
titanic$Sex[max(titanic$Age, na.rm=TRUE)]
table(titanic$Sex[titanic$Age==40])
table(titanic$Sex) / table(titanic$Sex[titanic$Age >= 18 & titanic$Age <= 35])
table(titanic$Sex) / table(titanic$Sex[titanic$Age < 17])

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))
params

titanic %>% ggplot(aes(sample = Age)) + 
  geom_qq(dparams = params) +
  geom_abline()

titanic %>% ggplot(aes(Survived, group=Sex, fill=Sex)) + geom_bar()

table(titanic$Sex, titanic$Survived)
p <- titanic[titanic$Sex=="male"]                   

titanic %>% ggplot(aes(Age, fill=Survived)) + geom_density(aes(y= ..count..), alpha=0.2)

table(titanic$Sex, titanic$Survived)          

titanic %>% filter(!Fare==0) %>% ggplot(aes(Fare, group=Survived, color=Survived)) + geom_boxplot() + scale_x_continuous("log2") + geom_jitter()

titanic %>% ggplot(aes(Pclass, fill=Survived)) + geom_bar()
titanic %>% ggplot(aes(Pclass, fill=Survived)) + geom_bar(position = position_fill())
titanic %>% ggplot(aes(Survived, fill=Pclass)) + geom_bar(position = position_fill())

table(titanic$Pclass)
216+184

titanic %>% 
  ggplot(aes(Age, fill=Survived)) + 
  geom_density(aes(y= ..count..), alpha=0.2, position = "stack") + 
  facet_grid(Sex ~ Pclass)


geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ .)

####Comprehensive Assessment
library(tidyverse)
library(dslabs)
library(ggrepel)
data(stars)
options(digits = 3)   # report 3 significant digits

mean(stars$magnitude)
sd(stars$magnitude)

stars %>% ggplot(aes(magnitude)) + geom_density()

stars %>% ggplot(aes(temp)) + geom_histogram()

stars %>% ggplot(aes(temp, fill=type)) + geom_boxplot() + coord_flip()

stars %>% ggplot(aes(temp, magnitude)) + geom_point()

stars %>% ggplot(aes(temp, magnitude, color=type, label=type)) + 
  geom_point() +
  geom_text() +
  geom_text_repel() +
  scale_y_reverse() + 
  scale_x_continuous((trans="log10")) + 
  scale_x_reverse() + 
  xlab("log10 temp")
  

stars %>% filter(star %in% c("Antares", "Castor", "Mirfak", "Polaris", "vanMaanen'sStar")) %>% 
  ggplot(aes(temp, magnitude, color=type, label=star)) +
  geom_point() + 
  scale_y_reverse() + 
  scale_x_continuous(trans="log10") + 
  geom_text() + 
  scale_x_reverse() + 
  xlab("log10 temp")

test <- stars %>% select(star, magnitude, temp) %>% filter(star %in% c("Antares", "Castor", "Mirfak", "Polaris", "van Maanen's Star"))

stars %>% filter(star %in% c("Rigel", "Deneb", "SiriusB", "van Maanen's Star", "Alnitak", "Alnitam", "Betelgeuse", "Antares", "Wolf359", "G51-I5")) %>% 
  ggplot(aes(temp, magnitude, color=type, label=star)) +
  geom_point() + 
  scale_y_reverse() + 
  scale_x_continuous(trans="log10") + 
  geom_text() + 
  scale_x_reverse() + 
  xlab("log10 temp")

####Comprehensive Assessment 2

library(tidyverse)
library(dslabs)
library(ggrepel)
library(ggplot2)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

temp_carbon %>% .$year %>% max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  max(year)

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  max(.$year)

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>% arrange(year)

temp_carbon$carbon_emissions[temp_carbon$year==2014] / temp_carbon$carbon_emissions[temp_carbon$year==1751]

temp_carbon %>% filter(!is.na(temp_anomaly)) %>% arrange(year) %>% select(year) %>% min()

temp_carbon %>% filter(!is.na(temp_anomaly)) %>% arrange(year) %>% select(year) %>% max()
                                                                        
temp_carbon$temp_anomaly[temp_carbon$year==2018] - temp_carbon$temp_anomaly[temp_carbon$year==1880]

p <- temp_carbon %>% filter(!is.na(temp_anomaly)) %>% ggplot(aes(year, temp_anomaly, ocean_anomaly)) + geom_line()

p + geom_hline(aes(yintercept = 0), col="blue")

p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue") + geom_hline(yintercept = 0.05, color="red") + geom_line(aes(y=ocean_anomaly), color="blue") + geom_line(aes(y=land_anomaly), color="green")
temp_carbon %>% filter(temp_anomaly>=0.5)

####Assessment 2####
library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

greenhouse_gases %>%
  ggplot(aes(year, concentration, color=gas)) +
  geom_line() +
  facet_grid(gas ~ ., scales = "free") +
  geom_vline(xintercept=1850, color="blue") +
  geom_hline(yintercept=275) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

temp_carbon %>% ggplot(aes(year, carbon_emissions)) + geom_line()

co2_time <- historic_co2 %>% ggplot(aes(year, co2, color=source)) + geom_line() + scale_x_continuous(labels=comma)
library(scales)
co2_time

