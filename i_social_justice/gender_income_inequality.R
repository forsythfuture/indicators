################################################################
# This file creates the data set for gender inequality in pay.
################################################################


# import custom ACS functions
# This loads the tidyverse and tidycensus packages,
# so they do not need to be loaded in this file
source('functions/acs_load_funcs.R')

table_number <- 'B20017'

gender_pay_data <- 'i_social_justice/data/gender_pay.csv'

# import B20017 table for counties
gender_pay_county <- ff_import_acs(geography = 'county', table_number, compare$state, compare$county, 
                                   2010, 2016, acs_data = 'acs/acs1')

# import B20017 table for NC
gender_pay_state <- ff_import_acs(geography = 'state', table_number, 'NC', county = NULL, 
                                  2010, 2016, acs_data = 'acs/acs1')

# import state-level data for every state
# this data can be used to create US-level data
gender_pay_us <- ff_import_acs('us', table_number, state=NULL, county = NULL, 
                                     2010, 2016, acs_data = 'acs/acs1')

# bind county, NC, and US datasets
gender_pay <- gender_pay_county %>%
  bind_rows(gender_pay_state) %>%
  bind_rows(gender_pay_us)

# save acs data in a csv file
#write_csv(gender_pay, gender_pay_data)

### In years following 2016, run this code to update the datasets

# # import master csv file of past data, and add current year data
# gender_update <- ff_update_county(gender_pay_data, table_number, 'NC', 'Forsyth', 2016)
# 
# # write out modified dataset that contains past and current data
# write_csv(gender_update, gender_pay_data)