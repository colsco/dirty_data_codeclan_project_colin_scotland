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


# time format 2017 ----
# Looks like the 2017 'internal_id' might be a time value in the wrong format;

candy_2017 <- candy_2017 %>% 
  
  mutate(timestamp = as.POSIXct(internal_id, tz = "", origin="2014-10-31"), .before = "internal_id") %>% 
  
  select(!contains("internal_id"))

# column names ----

# There are differences between the column names in each file.  The could do with some standardisation.

# First, find out which column names differ between 2015 and 2016;
# second%>%select(which(!(colnames(second) %in% colnames(first)))

candy_2016 %>% 
  select(which(!colnames(candy_2016) %in% colnames(candy_2015)))

# 30 columns have different names.  It looks like there's no gender column in 2015;

  "(?i)gender" %in% colnames(candy_2015)
  
  "(?i)sex" %in% colnames(candy_2015)

  # Both the above return false, so there isn't a gender question in 2015.  If it can be 
  # applied across all three data sets then it doesn't provide much analytical value here;
  
  candy_2016 <- candy_2016 %>% 
    select(!contains("gender"))
  
  candy_2017 <- candy_2017 %>% 
    select(!contains("gender"))
  
  # There are also numerous columns in 2015 and 2016 asking about peoples' perception of 
  # themselves against various famous people.  These don't provide us with any grounds 
  # for analysis in the context of Halloween sweets.  The questions are all phrased 
  # in terms of 'degrees_of_separation' and can be removed;
  
  candy_2015 <- candy_2015 %>% 
    select(!contains("_of_separation"))
  
  candy_2016 <- candy_2016 %>% 
    select(!contains("_of_separation"))
  
  # There are also some unnecessary questions regarding opinions on oter topics
  # unrelated to sweets.  These often start 'please_...' and can be removed.
  
  candy_2015 <- candy_2015 %>% 
    select(!contains("please_"))
  
  candy_2016 <- candy_2016 %>% 
    select(!contains("please_"))

  
  