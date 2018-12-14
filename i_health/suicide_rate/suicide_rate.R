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
  
age <- suicide %>%
  select(year, geo_description, demographic, `10_14`:`84_plus`) %>%
  filter(demographic %in% c('Total', 'TOTAL')) %>%
  # convert to long form where each row is a different age group
  gather('age', 'estimate', -year, -geo_description, -demographic) %>%
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
  bind_rows(., age) %>%
  # remove demographic of other race
  filter(!(type == 'Race / Ethnicity' & subtype == 'Other'))
  
### Import and clean population data

# import population data
population <- read_csv('population_data/population_cleaned.csv')

# create datasets that have the population by age, race, gender
agg_pop <- function(grouping, type_name, all = FALSE) {
  
  base_groups <- c('year', 'geo_description')
  group_cols <- if (grouping == 'Total') base_groups else append(base_groups, grouping) 
  
  df <- population %>%
    group_by_at(group_cols) %>%
    summarize(population = sum(population)) %>%
    mutate(type = type_name)
  
  df <- if (grouping == 'Total') mutate(df, subtype = !!grouping) else rename(df, subtype = !!grouping)
  
  return(df)
  
}

population <- agg_pop('gender', 'Gender') %>%
  bind_rows(agg_pop('race', 'Race / Ethnicity')) %>%
  bind_rows(agg_pop('age', 'Age')) %>%
  bind_rows(agg_pop('Total', 'Comparison Community'))

### Merge population data with suicide

# create common age bins between suicide and population
population <- population %>% ungroup() %>%
  mutate(subtype = str_replace_all(.$subtype, '[<] 1 year|1-4 years|5-9 years|10-14 years|15-19 years|20-24 years', 'Under 25')) %>%
  mutate(subtype = str_replace_all(.$subtype, '25-29 years|30-34 years|35-39 years|40-44 years', '25 to 44')) %>%
  mutate(subtype = str_replace_all(.$subtype, '45-49 years|50-54 years|55-59 years|60-64 years', '45 to 64')) %>%
  mutate(subtype = str_replace_all(.$subtype, '65-69 years|70-74 years|75-79 years|80-84 years|85[+] years', '65 and over')) %>%
  group_by(year, geo_description, type, subtype) %>%
  summarize(population = sum(population))

suicide <- suicide %>% ungroup() %>%
  mutate(subtype = str_replace_all(.$subtype, '10_14|15_19|20_24', 'Under 25')) %>%
  mutate(subtype = str_replace_all(.$subtype, '25_34|35_44', '25 to 44')) %>%
  mutate(subtype = str_replace_all(.$subtype, '45_54|55_64', '45 to 64')) %>%
  mutate(subtype = str_replace_all(.$subtype, '65_74|75_84|84_plus', '65 and over'))  %>%
  group_by(year, geo_description, type, subtype) %>%
  summarize(estimate = sum(estimate))

# merge population data to suicide data
suicide <- left_join(suicide, population, by = c('year', 'geo_description', 'type', 'subtype')) %>%
  # calculate suicide rate per 100,000
  mutate(rate = (estimate / population) * 100000,
         # se, formula taken from previous analysis
         # formula creates standard error based on Poisson distribution of counts
         se = sqrt(rate*100000/population)) %>%
  # change column names to match shiny format
  rename(success = estimate, trials = population, estimate = rate)
  
#write_csv(suicide, 'i_health/suicide_rate/suicide_shiny/suicide_rate.csv')
