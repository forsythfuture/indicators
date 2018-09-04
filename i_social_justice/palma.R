library(tidyverse)
library(data.table)

# Create housing variables

house_vars <- c('RT', 'SERIALNO', 'PUMA', 'ST', 'ADJINC', 'HINCP', 'WGTP')

# Create population variables

pop_vars <- c('SERIALNO', 'AGEP')

# create function to agjust for age based on equivalence scale
equivalence_scale <- function(num_adults, num_children) {
  
  ifelse(# if num adults is one or two, and no children
        num_adults <= 2 & num_children == 0, num_adults^0.5,
            ifelse(# if single parent
              num_adults == 1 & num_children >= 1,
                (num_adults + 0.8 * 1 + 0.5 * num_children-1)^0.7,
              # other families
              (num_adults + 0.5 * num_children)^0.7
            )
            )
  
}

### import data ###
housing <- fread('ss14husa.csv', select = house_vars) %>%
  # filter for housing units and for units with reported household income
  filter(RT == 'H',
         !is.null(HINCP)) %>%
  # merge housing with population data
  inner_join(fread('ss14pusa.csv', select = pop_vars),
             by = 'SERIALNO') %>%
  # create categorical variable on whether person is adult or child
  mutate(status = ifelse(.$AGEP >= 18, 'adult', 'child'))

# create data set that shows number of adults in household in one column
# and number of children in household in other column
house_num <- housing %>%
  # group by serial number (household)
  group_by(SERIALNO) %>%
  # count number of adults and children in household
  count(status) %>%
  # spread this information so that one column is num. adults and other is num. children
  spread(status, n) %>%
  # replace NA values (which represent 0 adults or children) with 0
  mutate(adult = replace_na(adult, 0),
         child = replace_na(child, 0)) %>%
  # replace NA values (which represent 0 adults or children) with 0
  mutate(adult = replace_na(adult, 0),
         child = replace_na(child, 0))


# add number of adults and children to primary housing data frame
housing <- housing %>%
  # drop columns in original data frame that are not needed since the other
  # data frame contains number of adults and children
  select(-AGEP, -status, -RT) %>%
  # drop duplicate entries, which gets rid of individuals
  distinct() %>%
  # add data frame that shows number of adults and children
  inner_join(house_num, by = 'SERIALNO') %>%
  # ungroup to save RAM
  ungroup()

# house_num no longer needed since it was joined to house
rm(house_num)
gc()
  

house_light <- head(housing, 100)

equivalence_scale(house_light$adult, house_light$child)
=======
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


>>>>>>> 1876aef0e0e95e54c578cbab2aac84a22c5c5420
