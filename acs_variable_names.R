########################################################################################################
# This file creates a dataframe of variable names for 1 year ACS tables, and exports the data frame into 
# a csv file. Creating a csv file of variable names will spead up programs because there will be no need 
# to repeatedly call the census api everytime a list of variable names is needed.
#
# This file should be updated to the current year when new ACS data drops.
#
########################################################################################################

library(tidyverse)
library(tidycensus)

current_year <- 2016

var_names <- load_variables(current_year, "acs1")

write_csv(var_names, 'acs_variable_names.csv')