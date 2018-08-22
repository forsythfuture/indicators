################################################################
# This file creates the data set for gender inequality in pay.
################################################################


# import custom ACS functions
# This loads the tidyverse and tidycensus packages,
# so they do not need to be loaded in this file
source('functions/acs_load_funcs.R')

table_number <- 'B20017'

gender_pay_data <- 'i_social_justice/data/gender_pay.csv'

# import B20017 table
gender_pay <- ff_import_acs(table_number, compare$state, compare$county, 2012, 2016)

# save acs data in a csv file
#write_csv(gender_pay, gender_pay_data)

### In years following 2016, run this code to update the datasets

# # import master csv file of past data, and add current year data
# gender_update <- ff_update_county(gender_pay_data, table_number, 'NC', 'Forsyth', 2016)
# 
# # write out modified dataset that contains past and current data
# write_csv(gender_update, gender_pay_data)