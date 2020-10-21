codes <- c("Italy"=380, "Germany"=124, "Spain"=818)
country <- c("Italy", "Germany", "Spain") 
names(codes)

seq(1,10)
seq(1,10, 2)
1:10

codes[2]
codes[c(1,3)]
codes[1:2]
codes["Germany"]
codes[c("Germany", "Italy")]

x <- c(1, "canada", 3)
class(x)
x <- 1:5
y <- as.character(x)
as.numeric(y)

x <- c(1,"b",3)
as.numeric(x)

library(dslabs)
data(murders)
sort(murders$total)
x <- c(31,4,15,92,65)
sort(x)
index <- order(x)
x[index]
murders$state[1:10]
murders$abb[1:10]

index <- order(murders$total)
murders$abb[index]

max(murders$total)
i_max <- which.max(murders$total)
murders$state[i_max]
i_min <- which.min(murders$total)
murders$state[i_min]

x <- c(31,4,15,92,65)
rank(x)


murders$state[which.max(murders$population)]

heights <- c(69,62,66,70,70,73,67,73,67,70)
heights * 2.54
heights -  69
murder_rate <- murders$total/murders$population*100000
murder_rate
murders$state[order(murder_rate, decreasing=TRUE)]
sort(murders$population, decreasing = FALSE)
order(murders$population)
rank(murders$population)
str(murders$population)

name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)