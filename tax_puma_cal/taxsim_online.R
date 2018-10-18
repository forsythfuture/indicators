##############################################################
#
# This script takes the dataset that was generated for use with the cli,
# and converts it to a dataset for use with the online platform
#
###############################################################

library(tidyverse)

taxes <- read_csv('tax_puma_cal/taxsim_datasets/taxes_2016.csv')
