library(tidyverse)

# import data
infant <- read_csv('i_health/infant_mortality/infant_mortality_raw_data.csv')

# break up into two datasets, one for rates and one for counts,
# then transform each dataset into long-form where each row is a different demographic / area / year combination,
# then recombine datasets into one
rates <- infant %>%
  select(year, geo_area, ends_with('rate')) %>%
  gather(key = 'demographic', value = 'mortality_rate', -year, -geo_area)

infant <- infant %>%
  select(year, geo_area, ends_with('deaths')) %>%
  gather(key = 'demographic', value = 'deaths', -year, -geo_area) %>%
  # add rates and convert to percentile
  mutate(rates = rates$mortality_rate,
         demographic = str_replace_all(demographic, '_deaths', ''),
         demographic = recode(demographic, 
                              white = 'White, non-Hispanic',
                              hisp = 'Hispanic/Latino',
                              aa = 'African American',
                              total = 'Total'),
         # derive total number of births from deaths and rate
         # formuala for infant mortality rate: (num deaths / total live births) * 1000
         births = round((deaths*1000)/rates, 0),
         # se, formula taken from previous analysis
         # formula creates standard error based on Poisson distribution of counts
         se = sqrt(rates*1000/births),
         # add type column that will either be race or total
         type = ifelse(demographic == 'Total', 
                       'Comparison Community', 'Race / Ethnicity')) %>%
  # rename columns to match standard type with Shiny app
  rename(subtype = demographic, geo_description = geo_area, 
         success = deaths, estimate = rates, trials = births) %>%
  # for comparison communities, only keep demographics for latest year
  filter(!(geo_description != "Forsyth County, NC" & subtype != "Total" & year != max(.$year)))

write_csv(infant, 'i_health/infant_mortality/infant_mortality_shiny/infant_mortality.csv')


