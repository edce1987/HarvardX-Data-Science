library(dslabs)
data(gapminder)

gapminder %>% filter(year== 2015 & country %in% c("Sri Lanka", "Turkey")) %>% select(country, infant_mortality)

ds_theme_set()
filter(gapminder, year==1962) %>% ggplot(aes(fertility, life_expectancy, color=continent)) + geom_point()

##########

filter(gapminder, year %in% c(1962, 2012)) %>% 
  ggplot(aes(fertility, life_expectancy, color=continent)) + 
  geom_point() + facet_grid(continent~year)

filter(gapminder, year %in% c(1960, 2012)) %>% 
  ggplot(aes(fertility, life_expectancy, color=continent)) + 
  geom_point() + facet_grid(.~year)

years <- c(1962,1980,1990,2000,2012)
continents <- c("Europe", "Asia")
gapminder %>% filter(year %in% years & continent %in% continents) %>% 
  ggplot(aes(fertility, life_expectancy, color=continent)) + 
  geom_point() + facet_wrap(~year)

##########

gapminder %>% filter(country=="United States") %>% ggplot(aes(year, fertility)) + geom_line()

countries <- c("South Korea", "Germany")
gapminder %>% filter(country %in% countries) %>% 
  ggplot(aes(year, fertility, group=country, color=country)) + 
  geom_line()

labels <- data.frame(country=countries, x= c(1975,1965), y=c(60,72))
countries <- c("South Korea", "Germany")
gapminder %>% filter(country %in% countries) %>% 
  ggplot(aes(year, life_expectancy, col=country)) + 
  geom_line() + 
  geom_text(data=labels, aes(x,y,label=country), size=5) + 
  theme(legend.position="none")

###########
gapminder <- gapminder %>% mutate(dollars_per_day=gdp/population/365)

past_year <- 1970
gapminder %>% 
  filter(year==past_year & !is.na(gdp)) %>% 
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth=1, color="black")

past_year <- 1963
gapminder %>% 
  filter(year==past_year & !is.na(gdp)) %>% 
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth=1, color="black") +
  scale_x_continuous(trans="log2")

########

length(levels(gapminder$region))

gapminder %>%
  filter(year==2000 & !is.na(gdp)) %>%
  mutate(region=reorder(region, dollars_per_day, FUN=median)) %>%
  ggplot(aes(region, dollars_per_day, fill=continent)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  xlab("") +
  scale_y_continuous(trans="log2") +
  geom_point(show.legend = FALSE)


# by default, factor order is alphabetical
fac <- factor(c("Asia", "Asia", "West", "West", "West"))
levels(fac)

# reorder factor by the category means
value <- c(10, 11, 12, 6, 4)
fac <- reorder(fac, value, FUN = mean)
levels(fac)

########

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
past_year <- 1970
present_year <- 2010
country_list_1 <- gapminder %>% filter(year== past_year & !is.na(dollars_per_day)) %>% .$country
country_list_2 <- gapminder %>% filter(year== present_year & !is.na(dollars_per_day)) %>% .$country
country_list <- intersect(country_list_1, country_list_2)
gapminder %>% 
  filter(year %in% c(past_year, present_year) & country %in% country_list & !is.na(gdp)) %>%
  mutate(group= ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color="black") +
  scale_x_continuous(trans="log2") +
  facet_grid(year~group)

p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + scale_y_continuous(trans = "log2")

p + geom_boxplot(aes(region, dollars_per_day, fill = continent)) +
  facet_grid(year ~ .)

p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + scale_y_continuous(trans = "log2")

p + geom_boxplot(aes(region, dollars_per_day, fill = continent)) +
  facet_grid(year ~ .)

# arrange matching boxplots next to each other, colored by year
p + geom_boxplot(aes(region, dollars_per_day, fill = factor(year)))

########

gapminder %>% 
  filter(year==past_year & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  group_by(group) %>%
  summarize(n=n()) %>%
  knitr::kable()

aes(x = dollars_per_day, y = ..count..)

gapminder %>% 
  filter(year==past_year & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  group_by(group) %>%
  summarize(n=n()) %>%
  knitr::kable()

p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day, y = ..count.., fill = group)) +
  scale_x_continuous(trans="log2")
p + geom_density(alpha = 0.2, bw = 0.75, position = "stack") + facet_grid(year ~.)

# add group as a factor, grouping regions
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))

# reorder factor levels
gapminder <- gapminder %>%
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))

# note you must redefine p with the new gapminder object first
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = "log2")

# stacked density plot
p + geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ .)

# weighted stacked density plot
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight = population/sum(population*2)) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, fill = group, weight = weight)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + facet_grid(year ~ .)

########

gapminder <- gapminder %>% 
  mutate(group = case_when(
    .$region %in% west ~"The West",
    .$region %in% "Northern Africa" ~"Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~"East Asia",
    .$region == "Southern Asia" ~"Southern Asia",
    .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"
))  

surv_income <- gapminder %>%
  filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarize(income = sum(gdp)/sum(population)/365, infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))

surv_income %>% arrange(income)

# plot infant survival versus income, with transformed axes
surv_income %>% ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
  scale_y_continuous(trans = "logit", limit = c(0.875, .9981),
                     breaks = c(.85, .90, .95, .99, .995, .998)) +
  geom_label(size = 3, show.legend = FALSE)

