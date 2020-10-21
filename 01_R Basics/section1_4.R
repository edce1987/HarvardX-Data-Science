a <- 0
if(a!=0){
  print(1/a)
} else{
  print("No reciprocal for 0.")
}
a <- 2

#General conditional
#if(boolean condition){expressions}
#else{alternative expressions}

library(dslabs)
data(murders)
murders$murder_rate <- murders$total/murders$population*100000

murders$state[murders$murder_rate<0.5]

ind <- which.min(murder_rate)
if(murder_rate[ind] < 0.05){
  print(murders$state[ind])
} else{
    print("No state has murder rate that low")
}

a <- 0
ifelse(a > 0, 1/a,NA)

a <- c(0,1,2,-4,5)
result <- ifelse(a>0,1/a,NA)
result

#replacing NA with other values

data("na_example")
sum(is.na(na_example))
no_nas <- ifelse(is.na(na_example), 0, na_example)
sum(no_nas)

z <- c(TRUE, TRUE, FALSE)
any(z)
all(z)

avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}
avg(x)
mean(x)

s <- 3
avg(1:10)

#general form
#my_function <- function(x){
#  operations that operate on x which is defined by user of function value final line is returned
#}

avg <- function(x, arithmetic=TRUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}

compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}
compute_s_n(10)

#general form
#for (i in range of values) {
#operations that use i, which is changing across the range of values
#}

for(i in 1:5){
  print(i)
}

m <- 25
#create an empty vector
s_n <- vector(length = m)
for(n in 1:m){
  s_n[n] <- compute_s_n(n)
}
n <- 1:m
plot(n,s_n)

lines(n, n*(n+1)/2)

#applyfamily -> learn important, split, cut, quantile

char_len <- nchar(murders$state)
head(char_len)

x <- c(2, 3, -999, 1, 4, 5, -999, 3, 2, 9)
x <- ifelse(x == -999, NA, x)

#lexical scooe
x <- 8
my_func <- function(y){
  x <- 9
  print(x)
  y + x
}
my_func(x)
print(x)

s_n <- vector("numeric", 25)
 
# write a for-loop to store the results in s_n
n <- 25
for (i in 1:n){
  x <- i
  s_n[i] <- compute_s_n(x)
}

mean(ifelse(heights$height > 72, heights$height, 0))

inches_to_ft <- function(x){
  +     y <- x/12
  }
inches_to_ft(144)
print(inches_to_ft(144))

factorial(5)

# define a vector of length m
m <- 10
f_n <- vector(length = m)

# make a vector of factorials
for (n in 1:m){
f_n[n] <- factorial(n)
}
# inspect f_n
f_n