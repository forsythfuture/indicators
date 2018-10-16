library(tidyverse)
library(DBI)

# connect to PUMS database
con <- dbConnect(RSQLite::SQLite(), "puma_data/pums_db.db")

# establish connection to tables
housing <- tbl(con, "h_16")
population <- tbl(con, "p_16")

# import these PUMS variables
house_vars <- c('RT', 'TYPE', 'SERIALNO', 'PUMA', 'ST', 'HINCP')
#house_weights <- c('WGTP', paste0('wgtp', seq(1, 80))) # replicate weight variables
#house_vars <- append(house_vars, house_weights) # combine house vars and weights
pop_vars <- c('SERIALNO', 'AGEP') # population variables

# import population data
pop <- population %>%
  select(!!pop_vars)


house <- housing %>%
  select(!!house_vars) %>%
  filter(PUMA == 1801, # Winston Salem
         ST == 37, # North Carolina (need both puma and state)
         TYPE == 1, # housing units only
         (!is.na(HINCP) & HINCP >= 0)) %>% # positive household income
  select(-TYPE) %>%
  # merge with population data
  left_join(pop, by = 'SERIALNO') %>%
  # convert age to either adult or femal
  mutate(AGEP = ifelse(AGEP >= 18, 'adult', 'child'))

# find number of adults and children for each family
# will be merge with the primary dataset
adults <- house %>%
  filter(AGEP == 'adult') %>%
  group_by(SERIALNO) %>%
  # count number of adults per household
  summarise(number_adults = n())
  
child <- house %>%
  filter(AGEP == 'child') %>%
  group_by(SERIALNO) %>%
  # count number of children per household
  summarise(number_child = n())

# merge primary dataset and number of adults and children
house <- house %>%
  left_join(adults, by = 'SERIALNO') %>%
  left_join(child, by = 'SERIALNO') %>%
  collect()

# calcuate age equivalency
house <- house %>%
  # replace NA values in numbers of aduls and children to 0
  mutate_at(vars(c('number_adults', 'number_child')), funs(replace_na(., 0))) %>%
  mutate(income = HINCP / equivalence_scale(number_adults, number_child))

  # replace missing values for number of adults and children with zero
  mutate(number_child = replace(number_child, is.na(number_child), 0)) %>%
  mutate(number_adults = replace(number_adults, is.na(number_adults), 0)) %>% 
  # create household size equivalency scale metric
  mutate(eq = ifelse(
    # if num adults is one or two, and no children
    number_adults <= 2 & number_child == 0, number_adults^0.5,
    ifelse(# if single parent
      number_adults == 1 & number_child >= 1,
      (number_adults + 0.8 * 1 + 0.5 * (number_child-1))^0.7,
      # other families
      (number_adults + 0.5 * number_child)^0.7
     )
    )
   ) %>%
  # divide equivalency scale by household income
  mutate(HINCPa = HINCP / eq) %>%
  collect()



  

rm(adults)
rm(child)
gc()
