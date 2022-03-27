# R script to clean and tidy ddata from dseabirds.xls

#Set up libraries for later use ----

library(tidyverse)
library(janitor)
library(readxl)
library(assertr)

# set the working directory for simpler path names ----

library(here)

here::here()

# Read in the data and clean up the column headers ----
# two sheets are needed from the .xls; "Bird data by record ID"
# and "Ship data by record ID";

bird_record <- read_xls("raw_data/seabirds.xls", sheet = "Bird data by record ID") %>% 
               clean_names()

ship_record <- read_xls("raw_data/seabirds.xls", sheet = "Ship data by record ID") %>% 
               clean_names()

# first look at the data ----

head(bird_record)
str(bird_record)

head(ship_record)
str(ship_record)

# data sheet join ----
# It looks like the data from these two sheets could be joined using 'record_id' as the primary key.
# Either data set on it's own doesn't give the full story, so it would be useful to only keep the 
# data that exists in *both* data sets, i.e. inner_join;

birds_joined <- inner_join(bird_record, ship_record, by = "record_id")

names(birds_joined)

# remove unnecessary columns ----
# There are a lot of columns in here that are unlikely to be needed in the analysis;

birds_joined <- birds_joined %>% 
  select(record_id, species_common_name_taxon_age_sex_plumage_phase, species_scientific_name_taxon_age_sex_plumage_phase, species_abbreviation, count, date, time, lat, long)

# clean up 'time' format ----
# The time column also contains unnecessary (and misleading) date information which needs to be tidied.
# All entries are currently prefixed with 1899-12-31;

  birds_joined <- birds_joined %>% 
  mutate(time = str_remove(time, "1899-12-31 "))
  
# check for lingerers;
  
  birds_joined %>% 
    summarise(sum(str_detect(time, "1899-12-31")))

# there are no more instances of 1899-12-31 and the 'time' column looks a lot more sensible.
  
# clean up 'date'----
  
  birds_joined <- birds_joined %>% 
    mutate(date = str_remove(date, " 00:00:00"))
  
  # check for lingerers;
  
  birds_joined %>% 
    summarise(sum(str_detect(date, " 00:00:00")))
  
# check 'counts' ----
  
  birds_joined %>% 
    slice_max(count, n = 10)
  
# Looks like there are a lot of very high counts (such as 99999);
  
  birds_max_count <-   birds_joined %>% 
    filter(count >= 10000)
  
# The high-count birds are Shearwaters and Prions, both of which are abundant 
# and nest in large colonies, so there may be good reason not to discount them at this stage.
  
  
# check for NAs ----
# Now that the columns have been stripped back to those of particular interest, how many NAs are here;

na_birds_joined <- birds_joined %>% 
  summarise(across(.fns = ~sum(is.na(.x))))

na_birds_joined

# There are several NAs here, but many are associated with [NO BIRDS RECORDED] entries.  These can be removed.

birds_joined <- birds_joined %>% 
   filter(species_common_name_taxon_age_sex_plumage_phase != "[NO BIRDS RECORDED]")

# What's this done to our NA count?

na_birds_joined <- birds_joined %>% 
  summarise(across(.fns = ~sum(is.na(.x))))

na_birds_joined

# There are many missing values in the 'count' column, which would seem important;


na_count_col <- birds_joined %>% 
  filter(is.na(count)) %>% 
  arrange(species_common_name_taxon_age_sex_plumage_phase)


# There are 2007 NAs in 'count', out of a total of over 48,000 rows.  We would not be able
# to impute counts with any degree of accuracy, so the best course of action would be to 
# remove these NAs.

birds_joined_no_na <- birds_joined %>% 
 drop_na(count)

# How does the NA situation look now;

na_birds_joined_no_na <- birds_joined_no_na %>% 
  summarise(across(.fns = ~sum(is.na(.x))))

# There are still missing values in scientific name;

birds_joined_no_na %>% 
  filter(is.na(species_scientific_name_taxon_age_sex_plumage_phase))

# Some scientific names are missing in these cases because the *exact* sub-species
# of a particular bird was not identified at the time.

# These could be left as NA for now, and any bird identification analysis can be done
# using one of the other identifier or name columns where there are no NAs.

# There are also some latitude and longitude NAs, but these rows still contain 
# bird identification and count values, so could still be used for numerical
# analysis where location is not necessary.

# clean up 'species' ----

#Some species names have been extended to include plumage information, meaning that
#trying to group_by species might result in one species having more than one row.
#It might make future life easier if we create a species column that doesn't 
#try to take plumage into account.

#With a new column;
#birds_joined_no_na <- birds_joined_no_na %>% 
#  mutate(.before = species_common_name_taxon_age_sex_plumage_phase,
#         species_base = str_remove(species_common_name_taxon_age_sex_plumage_phase, 
#                                   " [A-Z]* [A-Z]*$|[A-Z]*[0-9]*$"))
####################################################################################
birds_joined_no_na <- birds_joined_no_na %>% 
  mutate(species_common_name_taxon_age_sex_plumage_phase
         = str_remove(species_common_name_taxon_age_sex_plumage_phase, 
                      " [A-Z]*[0-9]*$"))
# Need to do the same replacement in the scientific name and abbreviation columns 
# to allow easier grouping by species later on;

birds_joined_no_na <- birds_joined_no_na %>% 
  mutate(species_scientific_name_taxon_age_sex_plumage_phase
       = str_remove(species_scientific_name_taxon_age_sex_plumage_phase, 
                        " [A-Z]*[0-9]*$"))

birds_joined_no_na <- birds_joined_no_na %>% 
  mutate(species_abbreviation
         = str_remove(species_abbreviation, " [A-Z]*[0-9]*$"))


# latitude longitude check ----
# check for valid latitude and longitude values; 
# latitude should be between +/- 90
# longitude between +/- 180

birds_joined_no_na %>% 
  filter(!is.na(lat)) %>% 
  verify(lat >= -90 & lat <= 90) 

birds_joined_no_na %>% 
  filter(!is.na(long)) %>% 
  verify(long >= -180 & long <= 180)

# So all lat/long values that are not NAs are valid 

# column rename ----
# Some columns could be renamed to make them a bit friendlier;

birds_joined_no_na <- birds_joined_no_na %>% 
  rename(common_name = species_common_name_taxon_age_sex_plumage_phase,
         scientific_name = species_scientific_name_taxon_age_sex_plumage_phase) 



# Save this as a clean data object ----

birds_clean <- birds_joined_no_na
