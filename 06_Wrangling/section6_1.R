#Key points
#Many datasets are stored in spreadsheets. A spreadsheet is essentially a file version of a data frame with rows and columns.
#Spreadsheets have rows separated by returns and columns separated by a delimiter. The most common delimiters are comma, semicolon, white space and tab.
#Many spreadsheets are raw text files and can be read with any basic text editor. However, some formats are proprietary and cannot be read with a text editor, such as Microsoft Excel files (.xls).
#Most import functions assume that the first row of a spreadsheet file is a header with column names. To know if the file has a header, it helps to look at the file with a text editor before trying to import it.

#Key points
#The working directory is where R looks for files and saves files by default.
#See your working directory with getwd(). Change your working directory with setwd().
#We suggest you create a directory for each project and keep your raw data inside that directory.
#Use the file.path() function to generate a full path from a relative path and a file name. Use file.path() instead of paste() because file.path() is aware of your operating system and will use the correct slashes to navigate your machine.
#The file.copy() function copies a file to a new path.

getwd()
setwd()

library(dslabs)

path <- system.file("extdata", package="dslabs")
list.files(path)

filename <- "murders.csv"
fullpath <- file.path(path, filename)
fullpath

file.copy(fullpath, getwd())
file.exists(filename)

####

#Key points
#readr is the tidyverse library that includes functions for reading data stored in text file spreadsheets into R. Functions in the package include read_csv(), read_tsv(), read_delim() and more. These differ by the delimiter they use to split columns.
#The readxl package provides functions to read Microsoft Excel formatted files.
#The excel_sheets() function gives the names of the sheets in the Excel file. These names are passed to the sheet argument for the readxl functions read_excel(), read_xls() and read_xlsx().
#The read_lines() function shows the first few lines of a file in R.

library(tidyverse)
library(dplyr)

read_lines("murders.csv", n_max = 3)

dat <- read_csv(filename)

####
#Key point
#R-base import functions (read.csv(), read.table(), read.delim()) generate data frames rather than tibbles and character variables are converted to factors. This can be avoided by setting the argument stringsAsFactors=FALSE.

filename <- "murders.csv"
dat2 <- read.csv(filename, stringsAsFactors=FALSE)
class(dat2)
class(dat2$abb)
class(dat2$region) 

####

#Key points
#The read_csv() function and other import functions can read a URL directly.
#If you want to have a local copy of the file, you can use download.file().
#tempdir() creates a directory with a name that is very unlikely not to be unique.
#tempfile() creates a character string that is likely to be a unique filename.

url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"

dat <- read_csv(url)
class(dat)

download.file(url, "murders.csv")

tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)

head(dat)

####

setwd("~/projects/")
setwd("/Users/edce/projects/")
getwd()

file.copy(file.path(path, "murders.csv"), getwd())
setwd("data")
file.copy(file.path(path, filename), getwd())
file.copy(file.path(path, "murders.csv"), file.path(getwd(), "data"))
file.location <- file.path(system.file("extdata", package = "dslabs"), "murders.csv")
file.destination <- file.path(getwd(), "data")
file.copy(file.location, file.destination) 

read_csv("HarvardX-Data-Science/times.csv")
read_csv("HarvardX-Data-Science/times.csv", col_names = TRUE)
read_delim("HarvardX-Data-Science/times.csv", delim = ",")

install.packages("readxl")

library(readr)


times2016 <- read_excel("times.xlsx", sheet = 2)
times2016 <- read_xlsx("times.xlsx", sheet = 2)

url <- "https://raw.githubusercontent.com/MyUserName/MyProject/master/MyData.csv "
dat <- read_csv(url)
download.file(url, "MyData.csv")

url2 <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"

dat <- read_table(url2)
dat <- read_csv(url2)
dat <- read_csv2(url2)
dat <- read_tsv(url2)

dat2 <- read_csv(url2, col_names = FALSE)

