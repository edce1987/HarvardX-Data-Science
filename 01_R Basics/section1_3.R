index <- murder_rate < 0.71
index <- murder_rate <= 0.71
index

murders$state[index]
sum(index)
TRUE & TRUE
FALSE & TRUE
FALSE & FALSE
west <- murders$region == "West"
safe <- murder_rate <= 1
index <- safe & west
murders$state[index]

x <- c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
which(x)
index <- which(murders$state =="Massachusetts")
index
murder_rate[index]

index <- match(c("New York", "Florida", "Texas"), murders$state)
index
murders$state[index]

x <- c("a", "b", "c", "d", "e")
y <- c("a","d","f")
y %in% x
c("Boston", "Dakota", "Washington") %in% murders$state

library(dplyr)

murders <- mutate(murders, rate=total/population*100000)
head(murders)

filter (murders, rate <= 0.71)

new_table <- select(murders, state, region, rate)
filter(new_table, rate <= 0.71)

murders %>% select(state, region, rate) %>% filter(rate <= 0.71)

grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"), 
                     exam_1 = c(95, 80, 90, 85), 
                     exam_2 = c(90, 85, 85, 90),
                     stringsAsFactors = FALSE)
class(grades$names)
grades

# Note that if you want ranks from highest to lowest you can take the negative and then compute the ranks 
x <- c(88, 100, 83, 92, 94)
rank(-x)

# Defining rate
rate <-  murders$total/ murders$population * 100000

# Redefine murders to include a column named rank
# with the ranks of rate from highest to lowest
murders <- mutate(murders, rank=rank(-rate))

# Create a new data frame called murders_nw with only the states from the northeast and the west
murders_nw <- filter(murders, region %in% c("Northeast", "West"))
# Number of states (rows) in this category 
nrow(murders_nw)

## Define the rate and rank column
murders <- mutate(murders, rate =  total / population * 100000, rank = rank(-rate))

# show the result and only include the state, rate, and rank columns, all in one line, in that order
filter(murders, region %in% c("Northeast", "West") & rate < 1) %>%  
  select(state, rate, rank)

# Loading the libraries
library(dplyr)
data(murders)

# Create new data frame called my_states (with specifications in the instructions)
my_states <- murders %>% mutate(rate =  total / population * 100000, rank = rank(-rate)) %>% filter(region %in% c("Northeast", "West") & rate < 1) %>% select(state, rate, rank)

population_in_millions <- murders$population/10^6
total_gun_murders <- murders$total
plot(population_in_millions, total_gun_murders)

hist(murders$rate)

murders$state[which.max(murders$rate)]

boxplot(rate~region, data=murders)

install.packages("ggplot2")

x <- c(1,2,-3,4)
if(all(x>0)){
  print("All Positives")
} else{
  print("Not All Positives")
}