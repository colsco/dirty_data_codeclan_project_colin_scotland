# Task 4: Halloween Candy Data
# Cleaning Script

# Library setup ----

library(tidyverse)
library(janitor)
library(here)
library(readxl)
library(lubridate)


# Read in the data and clean column names ----

candy_2015 <- read_xlsx("raw_data/candy_ranking_data/boing-boing-candy-2015.xlsx") %>% 
              clean_names()

candy_2016 <- read_xlsx("raw_data/candy_ranking_data/boing-boing-candy-2016.xlsx") %>% 
              clean_names()

candy_2017 <- read_xlsx("raw_data/candy_ranking_data/boing-boing-candy-2017.xlsx") %>% 
              clean_names()



# First look at the data ----

head(candy_2015)
names(candy_2015)
str(candy_2015)
glimpse(candy_2015)
class(candy_2015)

head(candy_2016)
names(candy_2016)
str(candy_2016)
glimpse(candy_2016)
class(candy_2016)

head(candy_2017)
names(candy_2017)
str(candy_2017)
glimpse(candy_2017)
class(candy_2017)

# 2015: 5630 rows x 124 columns
# 2016: 1259 rows x 123 columns
# 2017: 2460 rows x 121 columns

# Full join should create 9349 rows

# Looks like the 2017 'internal_id' might be a time value in the wrong format;

candy_2017 <- candy_2017 %>% 
  
  mutate(timestamp = as.POSIXct(internal_id, tz = "", origin="2014-10-31"), .before = "internal_id")

# Now all data can be joined into one data set.  Each data set contains information
# from a different year, so there will be no 'matches'.  A full join can be used to 
# retain all rows.

join_15_16 <- full_join(candy_2015, candy_2016, by = c("timestamp"))

join_15_16_17 <- full_join(join_15_16, candy_2017, by = "timestamp")

# Check we have the expected no. rows:

glimpse(join_15_16_17)


#Rows: 9,349 Columns: 366 ; row count looks good, however column names look like they could be better!


# Looks like the time data in 2017 might be inaccurate, but time is not important
# in the context of our analysis.



