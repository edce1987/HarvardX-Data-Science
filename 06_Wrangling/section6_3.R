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


