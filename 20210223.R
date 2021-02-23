library(tidyverse)
library(tidytuesdayR)

rm(list=ls(all=TRUE))


# Read in as a dataframe
earn <- as.data.frame(tt_load('2021-02-23')$"earn")
employed <- as.data.frame(tt_load('2021-02-23')$"employed")


# Exploratory data analyis
###employed
table(employed$industry)
# why Asian? 

table(employed$major_occupation)
# why so many 1488 counts?

table(employed$year)
table(employed$race_gender)

###earn
table(earn$year) # from 2010 to 2020

# Graph ideas 
# - share of men or women / race in occupations from 2015 to 2020
# - Dubois style graph... 

