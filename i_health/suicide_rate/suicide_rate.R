library(tidyverse)

suicide <- read_csv('i_health/suicide_rate/suicide_rate_raw.csv')

# demographics are in rows, and ages are in columns
# create two datasets, one for demographics, one for ages
demographics <- select(suicide, year, geo_description, demographic, TOTAL) %>%
  rename(suicides = TOTAL) %>%
  # filter out total rows, will add back after race and gender are created
  filter(demographic != 'TOTAL',
         demographic != 'Total') %>%
  # demographic column is cross-tab of race and gender
  # create sepeate columns for race and gender
  separate(demographic, into = c('race', 'gender'), sep = 1) %>%
  # currently race and gender are in seperate columns,
  # convert to long form where they are in separate rows
  # this makes grouping and summing by race and gender easier
  gather('type', 'subtype', gender, race) %>%
  group_by(year, geo_description, type, subtype) %>%
  summarize(estimate = sum(suicides)) %>%
  ungroup() %>%
  mutate(subtype = recode(subtype, 
                          `F` = 'Female',
                          M = 'Male',
                          B = 'African American',
                          O = 'Other',
                          W = 'White, non-Hispanic'),
         type = recode(type,
                       gender = 'Gender',
                       race = 'Race / Ethnicity'))
  
age <- select(suicide, year, geo_description, `10_14`:`84_plus`) %>%
  # convert to long form where each row is a different age group
  gather('age', 'estimate', -year, -geo_description) %>%
  # calculate total suicides per age group
  group_by(year, geo_description, age) %>%
  summarize(estimate = sum(estimate)) %>%
  mutate(type = 'Age') %>%
  rename(subtype = age)

# calculate total suicides for each geographic area
total <- age %>%
  group_by(year, geo_description) %>%
  summarize(estimate = sum(estimate)) %>%
  mutate(type = 'Comparison Community',
         subtype = 'Total')

# bind all data
suicide <- bind_rows(total, demographics) %>%
  bind_rows(., age)
  
### Merge with population data

# import population data
population <- read_csv('population_data/population_cleaned.csv')

# create datasets that have the population by age, race, gender
agg_pop <- function(grouping, type_name, all = FALSE) {
  
  base_groups <- c('year', 'geo_description')
  group_cols <- if (grouping == 'Total') base_groups else append(base_groups, grouping) 
  
  df <- population %>%
    group_by_at(group_cols) %>%
    summarize(population = sum(population)) %>%
    rename(subtype = grouping) %>%
    mutate(type = type_name)
  
  return(df)
  
}
group_columns <- c('year', 'geo_description', 'gender')

population <- agg_pop('gender', 'Gender') %>%
  bind_rows(agg_pop('race', 'Race / Ethnicity')) %>%
  bind_rows(agg_pop('age', 'Age')) #%>%
  #bind_rows(agg_pop('Total', 'Total'))

%>%
  filter(geo_description %in% c('North Carolina', 'Forsyth County, NC',
                                'Guilford County, NC', 'Durham County, NC'))
