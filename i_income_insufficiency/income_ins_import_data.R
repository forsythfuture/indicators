library(tidyverse)
library(readxl)

source('i_income_insufficiency/income_ins_functions.R')

####################### consumer expenditure survey #############################

########## data source ######################################################################
#
# All consumer expenditure survey (ces) data was gathered from the BLS website containing
# consumer expenditure data. The following expenses were collected fro mCES data: 
# health insurance (by household size annd age), transportation, appareal and services, housekeeping supplies
# personal care products and services, reading, and misc. 
#
# To collect all expenses except for health insurance by age, the one screen data source application
# was used (https://data.bls.gov/PDQWeb/cx). From this page, the 'Expenses' Category was selected.
# Next, the following subcategories were used 


# create path to folder with data (to shorten file names that have to be entered)
data_path <- 'i_income_insufficiency/data/ces/'

### health insurance for households
# health insurance for poeple over 65 is in a different table and is not included here
health_household <- clean_ces(paste0(data_path, 'health_ins_household_raw_data.txt'),
                              paste0(data_path, 'health_ins_household_headers.txt'))

### transportation
trans <- ff_clean_ces(paste0(data_path, 'transportation_raw_data.txt'),
                      paste0(data_path, 'transportation_headers.txt'))

### health insurance 65+
health_age <- read_excel(paste0(data_path, '/health_ins_age/health_ins_age_16.xlsx'), skip = 2) %>%
  # only select items column and two age columns
  select(Item, starts_with('65-74'), starts_with('75 years')) %>%
  # trim whitespace in all columns
  mutate_all(funs(str_trim(., side = 'both'))) %>%
  # only keep row showing health insurance expenses
  filter(Item == 'Health insurance')



