##################################################################
#
# This script takes as input individual tax liabilities, computes
# household tax liabilities, and merges with puma data.
#
##################################################################

library(tidyverse)

read_csv('taxes_2006.csv', col_names = FALSE)

a$X1 <- floor(a$X1)

write_csv(a, 'taxes_2006a.csv', col_names = FALSE)

# read in tax data
taxes <- read_delim('tax_puma_cal/nc_from_taxsim/calculated_2006.txt', delim = ' ') 

a <- taxes %>%
  # truncate id to remove decimal
  mutate(taxsim_id = floor(taxsim_id),
         # add federal and state tax to get overall taxes
         tax_liability = fiitax + siitax) %>%
  # group by id, so we can calculate household tax liability
  group_by(taxsim_id) %>%
  summarize(sum(tax_liability))
