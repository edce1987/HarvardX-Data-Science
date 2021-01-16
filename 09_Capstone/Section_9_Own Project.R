#Source: https://cran.r-project.org/web/packages/OECD/OECD.pdf

#Install required packages if not done already
install.packages("OECD")

#Load required libraries
library(OECD)
library(tidyverse)
library(caret)

#List available datasets
get_datasets()

ALFS_POP_VITAL Population and Vital Statistics
ALFS_POP_LABOUR Population and Labour Force
SNA_TABLE1 1. Gross domestic product (GDP)
LFS_SEXAGE_I_R LFS by sex and age - indicators
TOURISM_KEY_IND_PC Key tourism indicators
AEO11_OVERVIEW_CHAPTER1_FIG6_EN Figure 1.6: World economic growth
BLI2015 Better Life Index - Edition 2015
FOOD_WASTE Food Waste
AV_AN_WAGE Average annual wages
BLI2017 Better Life Index - Edition 2017

dataGDP <- get_dataset("SNA_TABLE1")
dataBLI <- get_dataset("BLI2019")

