#grid
#lattice
#ggplot2 works with data tables only

library(ggplot2)
library(dslabs)
data(murders)

ggplot(data=murders)
murders %>% ggplot

p <- ggplot(data=murders)
print(p)

?geom_point

murders %>% ggplot() + geom_point(aes(x = population/10^6, y = total))

ggplot(data=murders) + geom_point(aes(x = population/10^6, y = total))

p <- ggplot(data=murders)
p1 <- p + geom_point(aes(x=population/10^6, y = total), size = 3) + geom_text(aes(population/10^6, total, label=abb), nudge_x = 2.5)

args(ggplot)

p <- murders %>% ggplot(aes(population/10^6, total, label = abb))
p + geom_point(size = 3) + geom_text(nudge_x = 2.5)

p + geom_point(size = 3) + geom_text(aes(x=10, y=800, label = "Hello there!"))

#Creation of plot
p <- murders %>% ggplot(aes(population/10^6, total, label = abb))
r <- murders %>% summarize(rate=sum(total) / sum(population) * 10^6) %>% .$rate

p + geom_abline(intercept=log10(r), lty = 2, color = "darkgrey") + 
  geom_point(aes(col=region), size = 3) +
  geom_text_repel(nudge_x = 0.03) + 
  scale_x_log10() + 
  scale_y_log10() + 
  xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") + 
  ggtitle("US Gun Murders in US in 2020") + 
  scale_color_discrete(name = "Region") + 
  theme_economist()
  
ds_theme_set()                     
install.packages("ggthemes")
install.packages("ggrepel")
library(ggthemes)
library(ggrepel)
p + theme_economist()


## Step by step
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(dslabs)
data(murders)

# define the intercept
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  .$rate

# make the plot, combining all elements
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()

######
library(dslabs)

data(heights)

p <- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))

p + geom_histogram(binwidth=1, fill = "red", col = "black") + 
  ggtitle("Histogram") + xlab("Height of male students")

#For QQ Plots
p + geom_density(fill="red")
p <- heights %>% filter(sex=="Male") %>% ggplot(aes(sample=height))
p + geom_qq()

params <- heights %>% filter(sex=="Male") %>% summarize(mean=mean(height), sd=sd(height))
p + geom_qq(dparams = params) + geom_abline()

# QQ-plot of scaled data against the standard normal distribution
heights %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()

# define plots p1, p2, p3
p <- heights %>% filter(sex == "Male") %>% ggplot(aes(x = height))
p1 <- p + geom_histogram(binwidth = 1, fill = "blue", col = "black")
p2 <- p + geom_histogram(binwidth = 2, fill = "blue", col = "black")
p3 <- p + geom_histogram(binwidth = 3, fill = "blue", col = "black")

# arrange plots next to each other in 1 row, 3 columns
install.packages("gridExtra")
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)
