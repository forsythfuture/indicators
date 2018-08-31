library(tidyverse)
library(pryr)
library(data.table)

# variables to import

housing_cols <- c(
                  'SERIALNO', # household #
                  'PUMA',
                  'ST',
                  'ADJINC', # income adjustment
                  'HINCP', # household income
                  'TYPE' # household or group quarters
                  )

pop_cols <- c('SERIALNO', 'AGEP')

# import and merge needed data

house <- fread('housing2011a.csv', select = housing_cols) %>% #import initial housing dataset
  # join weights to housing dataset
  left_join(fread('housingwts2011a.csv', select = c('SERIALNO', 'WGTP')),
            by = 'SERIALNO') %>%
  # inner join with population dataset
  inner_join(fread('population2011a.csv', select = pop_cols),
             by = 'SERIALNO') %>%
  # remove if type is not household (TYPE == 1)
  filter(TYPE == 1) %>%
  # create new column that labels people over 17 as adults and 17 and under as child
  mutate(status = if_else(.$AGEP <= 17, 'child', 'adult')) %>%
  # drop age and type because no longer needed
  select(-TYPE, -AGEP)

# create data frame that counts the number of children and join with primary
house_num <- house %>%
  # group by household
  group_by(SERIALNO) %>%
  # count the number of adults and children within each household
  count(status) %>% 
  # create separate columns for number of adults and number of children
  spread(status, n) %>%
  # join with main dataframe
  inner_join(house, by = 'SERIALNO') %>%
  # drop unneeded row 
  # (status not needed because there are now columns for number of adults and children)
  select(-status) %>%
  # remove duplicates
  distinct() %>%
  # ungroup to save RAM
  ungroup()


