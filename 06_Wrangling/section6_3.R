#Key points
#The most common tasks in string processing include:
#  extracting numbers from strings
#removing unwanted characters from text
#finding and replacing characters
#extracting specific parts of strings
#converting free form text to more uniform formats
#splitting strings into multiple values
#The stringr package in the tidyverse contains string processing functions that follow a similar naming format (str_functionname) and are compatible with the pipe.

# read in raw murders data from Wikipedia
url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
murders_raw <- read_html(url) %>% 
  html_nodes("table") %>% 
  html_table() %>%
  .[[1]] %>%
  setNames(c("state", "population", "total", "murder_rate"))

# inspect data and column classes
head(murders_raw)
class(murders_raw$population)
class(murders_raw$total)

##
#Key points
#Define a string by surrounding text with either single quotes or double quotes.
#To include a single quote inside a string, use double quotes on the outside. To include a double quote inside a string, use single quotes on the outside.
#The cat() function displays a string as it is represented inside R.
#To include a double quote inside of a string surrounded by double quotes, use the backslash (\) to escape the double quote. Escape a single quote to include it inside of a string defined by single quotes.
#We will see additional uses of the escape later.

s <- "Hello!"    # double quotes define a string
s <- 'Hello!'    # single quotes define a string
s <- `Hello`    # backquotes do not

#s <- "10""    # error - unclosed quotes
s <- '10"'    # correct

# cat shows what the string actually looks like inside R
cat(s)

s <- "5'"
cat(s)

# to include both single and double quotes in string, escape with \
#s <- '5'10"'    # error
#s <- "5'10""    # error
s <- '5\'10"'    # correct
cat(s)
s <- "5'10\""    # correct
cat(s)

##
#Key points
#The main types of string processing tasks are detecting, locating, extracting and replacing elements of strings.
#The stringr package from the tidyverse includes a variety of string processing functions that begin with str_ and take the string as the first argument, which makes them compatible with the pipe.
#Code
# murders_raw defined in web scraping video

# direct conversion to numeric fails because of commas
murders_raw$population[1:3]
as.numeric(murders_raw$population[1:3])

library(tidyverse)    # includes stringr

##
#Key points
#Use the str_detect() function to determine whether a string contains a certain pattern.
#Use the str_replace_all() function to replace all instances of one pattern with another pattern. To remove a pattern, replace with the empty string ("").
#The parse_number() function removes punctuation from strings and converts them to numeric.
#mutate_at() performs the same transformation on the specified column numbers.
#Code
# murders_raw was defined in the web scraping section

# detect whether there are commas
commas <- function(x) any(str_detect(x, ","))
murders_raw %>% summarize_all(funs(commas))

# replace commas with the empty string and convert to numeric
test_1 <- str_replace_all(murders_raw$population, ",", "")
test_1 <- as.numeric(test_1)

# parse_number also removes commas and converts to numeric
test_2 <- parse_number(murders_raw$population)
identical(test_1, test_2)

murders_new <- murders_raw %>% mutate_at(2:3, parse_number)
murders_new %>% head

####
#Key points
#In the raw heights data, many students did not report their height as the number of inches as requested. There are many entries with real height information but in the wrong format, which we can extract with string processing. 
#When there are both text and numeric entries in a column, the column will be a character vector. Converting this column to numeric will result in NAs for some entries.
#To correct problematic entries, look for patterns that are shared across large numbers of entries, then define rules that identify those patterns and use these rules to write string processing tasks.
#Use suppressWarnings() to hide warning messages for a function.

library(dslabs)
library(tidyverse)
data("reported_heights")

class(reported_heights$height)
x <- as.numeric(reported_heights$height)
head(x)
sum(is.na(x))

# keep only entries that result in NAs
reported_heights %>% mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>% 
  head(n=10)

not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

problems <- reported_heights %>%
  filter(not_inches(height)) %>%
  .$height
length(problems)  

# 10 examples of x'y or x'y" or x'y\"
pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 10 examples of x.y or x,y
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 10 examples of entries in cm rather than inches
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81) )
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat

#Key points
#A regular expression (regex) is a way to describe a specific pattern of characters of text. A set of rules has been designed to do this specifically and efficiently.
#stringr functions can take a regex as a pattern.
#str_detect() indicates whether a pattern is present in a string.
#The main difference between a regex and a regular string is that a regex can include special characters.
#The | symbol inside a regex means "or".
#Use '\\d' to represent digits. The backlash is used to distinguish it from the character 'd'. In R, you must use two backslashes for digits in regular expressions; in some other languages, you will only use one backslash for regex special characters.
#str_view() highlights the first occurrence of a pattern, and the str_view_all() function highlights all occurrences of the pattern.

pattern <- ","
str_detect(murders_raw$total, pattern)

str_subset(reported_heights$height, "cm")

yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)

str_detect(s, "cm") | str_detect(s, "inches")

str_detect(s, "cm|inches")

# use the "or" symbol inside a regex (|)
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)
str_detect(s, "cm") | str_detect(s, "inches")
str_detect(s, "cm|inches")

# highlight the first occurrence of a pattern
str_view(s, pattern)

# highlight all instances of a pattern
str_view_all(s, pattern)

#Key points
#Define strings to test your regular expressions, including some elements that match and some that do not. This allows you to check for the two types of errors: failing to match and matching incorrectly.
#Square brackets define character classes: groups of characters that count as matching the pattern. You can use ranges to define character classes, such as [0-9] for digits and [a-zA-Z] for all letters.
#Anchors define patterns that must start or end at specific places. ^ and $ represent the beginning and end of the string respectively.
#Curly braces are quantifiers that state how many times a certain character can be repeated in the pattern. \\d{1,2} matches exactly 1 or 2 consecutive digits.

# s was defined in the previous video
yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)
pattern <- "\\d"

# [56] means 5 or 6
str_view(s, "[56]")

# [4-7] means 4, 5, 6 or 7
yes <- as.character(4:7)
no <- as.character(1:3)
s <- c(yes, no)
str_detect(s, "[4-7]")

# ^ means start of string, $ means end of string
pattern <- "^\\d$"
yes <- c("1", "5", "9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
str_view(s, pattern)

# curly braces define quantifiers: 1 or 2 digits 
pattern <- "^\\d{1,2}$"
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b")
str_view(c(yes, no), pattern)

# combining character class, anchors and quantifier
pattern <- "^[4-7]'\\d{1,2}\"$"
yes <- c("5'7\"", "6'2\"",  "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
str_detect(yes, pattern)
str_detect(no, pattern)

####
#Key points
#str_replace() replaces the first instance of the detected pattern with a specified string.
#Spaces are characters and R does not ignore them. Spaces are specified by the special character \\s.
#Additional quantifiers include *, + and ?. * means 0 or more instances of the previous character. ? means 0 or 1 instances. + means 1 or more instances.
#Before removing characters from strings with functions like str_replace() and str_replace_all(), consider whether that replacement would have unintended effects.

# number of entries matching our desired pattern
pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern))

# inspect examples of entries with problems
problems[c(2, 10, 11, 12, 15)] %>% str_view(pattern)
str_subset(problems, "inches")
str_subset(problems, "''")

# replace or remove feet/inches words before matching
pattern <- "^[4-7]'\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()

# R does not ignore whitespace
identical("Hi", "Hi ")

# \\s represents whitespace
pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems, pattern_2)

# * means 0 or more instances of a character
yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")
str_detect(yes, "A1*B")
str_detect(no, "A1*B")

# test how *, ? and + differ
data.frame(string = c("AB", "A1B", "A11B", "A111B", "A1111B"),
           none_or_more = str_detect(yes, "A1*B"),
           nore_or_once = str_detect(yes, "A1?B"),
           once_or_more = str_detect(yes, "A1+B"))

# update pattern by adding optional spaces before and after feet symbol
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()

##
#Key Points
#Groups are defined using parentheses.
#Once we define groups, we can use the function str_match() to extract the values these groups define. str_extract() extracts only strings that match a pattern, not the values defined by groups.
#You can refer to the ith group with \\i. For example, refer to the value in the second group with \\2.

# define regex with and without groups
pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <-  "^([4-7]),(\\d*)$"

# create examples
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)

# demonstrate the effect of groups
str_detect(s, pattern_without_groups)
str_detect(s, pattern_with_groups)

# demonstrate difference between str_match and str_extract
str_match(s, pattern_with_groups)
str_extract(s, pattern_with_groups)

# improve the pattern to recognize more events
pattern_with_groups <-  "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_replace(s, pattern_with_groups, "\\1'\\2")

# final pattern
pattern_with_groups <-"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"

# combine stringr commands with the pipe
str_subset(problems, pattern_with_groups) %>% head
str_subset(problems, pattern_with_groups) %>% 
  str_replace(pattern_with_groups, "\\1'\\2") %>% head

#Key points
#Wrangling with regular expressions is often an iterative process of testing the approach, looking for problematic entries, and improving the patterns.
#Use the pipe to connect stringr functions.
#It may not be worth writing code to correct every unique problem in the data, but string processing techniques are flexible enough for most needs.

# function to detect entries with problems
not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}

# identify entries with problems
problems <- reported_heights %>% 
  filter(not_inches_or_cm(height)) %>%
  .$height
length(problems)

converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
  str_replace("inches|in|''|\"", "") %>%  #remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") ##change format

# find proportion of entries that fit the pattern after reformatting
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)

converted[!index]    # show problems

library(tidyverse)


####Assessment string processing
not_inches <- function(x, smallest = 50, tallest = 84) {
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest 
  ind
}

not_inches(85)

#
s <- c("70", "5 ft", "4'11", "", ".", "Six feet")
pattern <- "\\d|ft"
str_view_all(s, pattern)

#
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]"
str_detect(animals, pattern)

#
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[A-Z]$"
str_detect(animals, pattern)

#

animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]{4,5}"
str_detect(animals, pattern)

#
animals <- c("moose", "monkey", "meerkat", "mountain lion")
pattern <- "mo?"
str_detect(animals, pattern)

#
schools <- c("U. Kentucky", "Univ New Hampshire", "Univ. of Massachusetts", "University Georgia", "U California", "California State University")
schools

schools %>% 
  str_replace("^Univ\\.?\\s|^U\\.?\\s", "University ") %>% 
  str_replace("^University of |^University ", "University of ")

#
problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

#
problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

#
converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% 
  str_replace("inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
converted[!index]

#
yes <- c("5 feet 7inches", "5 7")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)

converted <- s %>% 
  str_replace("feet|foot|ft", "'") %>% 
  str_replace("inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)

#Corrected version
converted <- s %>% 
  str_replace("\\s*(feet|foot|ft)\\s*", "'") %>% 
  str_replace("\\s*(inches|in|''|\")\\s*", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)

####

