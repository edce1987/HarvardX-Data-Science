library(tidyverse)
library(dslabs)
data(gapminder)

# create and inspect a tidy data frame
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)
head(tidy_data)

# plotting tidy data is simple
tidy_data %>% 
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

# import and inspect example of original Gapminder data in wide format
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
select(wide_data, country, `1960`:`1967`)

####

new_tidy_data <- wide_data %>%
  gather(year,fertility, `1960`:`2015`)
head(new_tidy_data)

new_tidy_data <- wide_data %>%
  gather(year, fertility, -country)
head(new_tidy_data)

class(tidy_data$year)
class(new_tidy_data$year)

#With conversion to numeric
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert=TRUE)
class(new_tidy_data$year)

#Plot
new_tidy_data %>% 
  ggplot(aes(year, fertility, color=country)) + 
  geom_point()

#Spread
new_wide_data <- new_tidy_data %>%
  spread(year, fertility)
select(new_wide_data, country, `1960`:`1967`)

####
#Key points
#The separate() function splits one column into two or more columns at a specified character that separates the variables.
#When there is an extra separation in some of the entries, use fill="right" to pad missing values with NAs, or use extra="merge" to keep extra elements together.
#The unite() function combines two columns and adds a separating character.

path <- system.file("extdata", package="dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename)
head(raw_dat)

dat <- raw_dat %>%
  gather(key, value, -country)
head(dat)

dat$key[1:5]

dat %>% separate(key, c("year", "variable_name"), "_")

#With correct separation
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right")

dat %>% 
  separate(key, c("year", "first_variable_name"), sep = "_", extra = "merge")

#With spread
dat %>%
  separate(key, c("year", "variable_name"), sep = "_", extra = "merge") %>%
  spread(variable_name, value)
 
plot <- dat %>%
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep = "_") %>%
  spread(variable_name, value) %>%
  rename(fertility = fertility_NA)

plot %>% 
  ggplot(aes(year, life_expectancy, color=country, group=country)) + 
  geom_smooth()

####Test

data <- read_csv("06_Wrangling/times.csv")

tidy_data <- data %>%
  gather(year, time, `2015`:`2017`)

tidy_data %>% spread(year, time)
     
tidy_data <- data %>%
  gather(key = "key", value = "value", -age_group) %>%
  separate(col = key, into = c("year", "variable_name"), sep = "_") %>% 
  spread(key = variable_name, value = value)

##

stats <- read_csv("06_Wrangling/stats.csv")

tidy_data <- stats %>%
  separate(col = key, into = c("player", "variable_name"), sep = "_", extra = "merge") %>% 
  spread(key = variable_name, value = value)

####Test 2

library(tidyverse)
library(dslabs)

head(co2)

##

co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

co2_tidy <- gather(co2_wide, month, co2, -year)

##

co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

##

library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)

dat_tidy <- spread(dat, gender, admitted)

##

tmp <- gather(admissions, key, value, admitted:applicants)
tmp

tmp2 <- unite(tmp, column_name, c(key, gender))

####
#Key points
#The join functions in the dplyr package combine two tables such that matching rows are together.
#left_join() only keeps rows that have information in the first table.
#right_join() only keeps rows that have information in the second table.
#inner_join() only keeps rows that have information in both tables.
#full_join() keeps all rows from both tables.
#semi_join() keeps the part of first table for which we have information in the second.
#anti_join() keeps the elements of the first table for which there is no information in the second.

# import US murders data
library(tidyverse)
library(ggrepel)
library(dslabs)
ds_theme_set()
data(murders)
head(murders)

# import US election results data
data(polls_us_election_2016)
head(results_us_election_2016)
identical(results_us_election_2016$state, murders$state)

tab <-left_join(murders, results_us_election_2016, by = "state")

tab %>% ggplot(aes(population/10^6, electoral_votes, label=abb)) +
  geom_point() +
  geom_text_repel() +
  scale_x_continuous(trans="log2") +
  scale_y_continuous(trans="log2") +
  geom_smooth(method = "lm", se=FALSE)

tab1 <- slice(murders, 1:6) %>%
  select(state, population)
tab1  

tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>%
  select(state, electoral_votes)
tab2

left_join(tab1, tab2)

tab1 %>% left_join(tab2)

tab1 %>% right_join(tab2)

tab1 %>% inner_join(tab2)

inner_join(tab1, tab2)

full_join(tab1, tab2)

semi_join(tab1, tab2)

anti_join(tab1, tab2)

####Binding

bind_cols(a=1:3, b=4:6) #cbind() 

tab1 <- tab[, 1:3]
tab2 <- tab[, 4:6]
tab3 <- tab[, 7:9]
new_tab <- bind_cols(tab1, tab2, tab3)
head(new_tab)

tab4 <- tab[1:2,]
tab5 <- tab[3:4,]
bind_rows(tab4, tab5) #rbind

####
#Key points
#By default, the set operators in R-base work on vectors. If tidyverse/dplyr are loaded, they also work on data frames.
#You can take intersections of vectors using intersect(). This returns the elements common to both sets.
#You can take the union of vectors using union(). This returns the elements that are in either set.
#The set difference between a first and second argument can be obtained with setdiff(). Note that this function is not symmetric.
#The function set_equal() tells us if two sets are the same, regardless of the order of elements.

# intersect vectors or data frames
intersect(1:10, 6:15)
intersect(c("a","b","c"), c("b","c","d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
intersect(tab1, tab2)

# perform a union of vectors or data frames
union(1:10, 6:15)
union(c("a","b","c"), c("b","c","d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
union(tab1, tab2)

# set difference of vectors or data frames
setdiff(1:10, 6:15)
setdiff(6:15, 1:10)
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
setdiff(tab1, tab2)

# setequal determines whether sets have the same elements, regardless of order
setequal(1:5, 1:6)
setequal(1:5, 5:1)
setequal(tab1, tab2)

####Assessment
install.packages("Lahman")
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()

Master %>% as_tibble()
Master
top_names <- top %>% left_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)

head(Salaries)
dim(top_names)
top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)

##

head(AwardsPlayers)

awards <- AwardsPlayers %>% filter(yearID==2016)
pot <- inner_join(top_names, awards)

pot2 <- anti_join(awards, top_names) %>% group_by(playerID) %>% summarize(n=n())

a <- awards %>% group_by(playerID) %>% summarize(n=n())
