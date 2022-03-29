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
dim(candy_2015)

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


# parse the junk: 2015 ----

removals <- c("please_", 
              "_separation_",
              "fill_in_the_blank", 
              "guess_the_number",
              "betty_or_veronica",
              "check_all_that_apply",
              "that_dress_that_went_viral",
              "your_favourite_font",
              "squint_really_hard",
              "which_day_do_you_prefer",
              "vicodin",                         # vicodin is an opioid
              "person_of_interest",
              "pencils",
              "bonkers_the_board_game",
              "bottle_caps",
              "cash_",
              "creepy_religious",
              "dental_",
              "acetaminophen",
              "glow_stick",
              "healthy_fruit",
              "physical_hugs",
              "joy_joy_mit_iodine",              # from the Simpsons...
              "kale_smoothie",
              "trail_mix",
              "do_you_eat_apples",
              "whole_wheat",
              "white_bread",
              "vials_of_pure_high_fructose",
              "q7_",
              "q8_",
              "q9_",
              "q10_",
              "q11_",
              "q12_",
              "coordinates",
              "x114",
              "chardonnay"
              )

candy_2015 <- candy_2015 %>% 
  select(!contains(removals))
         
# parse the junk 2016 ----

candy_2016 <- candy_2016 %>% 
  select(!contains(removals))

# parse the junk 2017 ----

candy_2017 <- candy_2017 %>% 
  select(!contains(removals))

# dim summary after parsing ----

# 2015: 5630 rows x 81 columns
# 2016: 1259 rows x 87 columns
# 2017: 2460 rows x 90 columns

# 2017 colnames cleanup

candy_2017 <- candy_2017 %>% 
  rename_all(~ str_replace(., regex("^[Qq][0-9]_"), ""))


