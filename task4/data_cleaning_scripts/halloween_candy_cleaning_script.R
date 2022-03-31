# Task 4: Halloween Candy Data
# Cleaning Script

# Library setup ----

library(tidyverse)
library(janitor)
library(here)
library(readxl)
library(lubridate)


# Read in the data and clean column names ----

candy_2015 <- read_xlsx(here("raw_data/candy_ranking_data/boing-boing-candy-2015.xlsx")) %>% 
              clean_names()

candy_2016 <- read_xlsx(here("raw_data/candy_ranking_data/boing-boing-candy-2016.xlsx")) %>% 
              clean_names()

candy_2017 <- read_xlsx(here("raw_data/candy_ranking_data/boing-boing-candy-2017.xlsx")) %>% 
              clean_names()


# First look at the data ----

#head(candy_2015)
#names(candy_2015)
#str(candy_2015)
#glimpse(candy_2015)
#class(candy_2015)
#dim(candy_2015)

#head(candy_2016)
#names(candy_2016)
#str(candy_2016)
#glimpse(candy_2016)
#class(candy_2016)

#head(candy_2017)
#names(candy_2017)
#str(candy_2017)
#glimpse(candy_2017)
#class(candy_2017)

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
              "chardonnay",
              "state_province_county",
              "abstained",
              "real_housewives",
              "ignore",
              "lapel_pins",
              "chalk",
              "mint_leaves",
              "any_full_size"
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
# 2016: 1259 rows x 85 columns
# 2017: 2460 rows x 88 columns

# 2017 colnames cleanup

candy_2017 <- candy_2017 %>% 
  rename_all(~ str_replace(., regex("^[Qq][0-9]_"), ""))

# mutate 'timestamp' --> 'year' ----

candy_2015 <- candy_2015 %>% 
  mutate(year = year(timestamp), .before = "timestamp") %>% 
  select(!timestamp)

candy_2016 <- candy_2016 %>% 
  mutate(year = year(timestamp), .before = "timestamp") %>% 
  select(!timestamp)

candy_2017 <- candy_2017 %>% 
  mutate(year = year(timestamp), .before = "timestamp") %>% 
  select(!timestamp)

# start renaming columns to ease join ---- 

# 2015 and 2016 have the most similar column names, so start there.  Standardise column order;

candy_2016 <- candy_2016 %>% 
  relocate(how_old_are_you, .after = "year") %>% 
  rename("bonkers" = "bonkers_the_candy") %>% 
  rename("box_o_raisins" = "boxo_raisins") %>% 
  rename("sweetums" = "sweetums_a_friend_to_diabetes") %>% 
  rename("gender" = "your_gender")

candy_2015 <- candy_2015 %>% 
  relocate(butterfinger, .after = "box_o_raisins") %>% 
  relocate(caramellos, .before = "chiclets")

# 2015 <--> 2016 join ----

# Will they join now?

#compare_df_cols(candy_2015, candy_2016)

# Looks like a join will be possible.  Use full_join to keep all data.

candy_15_16_full <- full_join(candy_2015, candy_2016)

# rename columns for 2017 join ----

candy_15_16_full <- candy_15_16_full %>% 
  rename("going_out" = "are_you_going_actually_going_trick_or_treating_yourself") %>% 
  rename("age" = "how_old_are_you")

candy_15_16_full <- candy_15_16_full %>% 
  relocate("which_country_do_you_live_in", .after = "age") %>% 
  rename("country" = "which_country_do_you_live_in")

candy_2017 <- candy_2017 %>% 
  relocate("age", .after = "year")

candy_2017 <- candy_2017 %>% 
  relocate("country", .after = "age")
  
# Will they join now?

# compare_df_cols(candy_15_16_full, candy_2017)

# not too far away

candy_2017 <- candy_2017 %>% 
  rename("brown_globs" = "anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes") %>% 
  rename("x100_grand_bar" = "100_grand_bar") %>% 
  rename("bonkers" = "bonkers_the_candy") %>% 
  rename("box_o_raisins" = "boxo_raisins") %>% 
  rename("sweetums" = "sweetums_a_friend_to_diabetes")

candy_15_16_full <- candy_15_16_full %>% 
  rename("brown_globs" = "anonymous_brown_globs_that_come_in_black_and_orange_wrappers")


 compare_df_cols(candy_15_16_full, candy_2017)

# final join ----

# full join again to keep all data

candy_all <- full_join(candy_15_16_full, candy_2017)
 
 candy_all <- candy_all %>%
   relocate("gender", .after = "age")

# NAs ----
 
candy_all %>% 
   summarise(across(.fns = ~sum(is.na(.x))))
 
# Quite a lot of NAs, but it looks like they shouldn't interfere with analysis too much.
 
# write to .csv
 
 candy_all <- candy_all %>% 
   write_csv(here("clean_data/candy_clean.csv"))

# clean up environment
  
  rm(candy_15_16_full)
 
  rm(candy_2015)
 
  rm(candy_2016)
 
  rm(candy_2017)
  
  rm(removals)


