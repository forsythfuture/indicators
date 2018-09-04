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