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
                              hisp = 'Hispanic / Latino',
                              aa = 'African American'),
         # derive total number of births from deaths and rate
         # formuala for infant mortality rate: (num deaths / total live births) * 1000
         births = round((deaths*1000)/rates), 0,
         # calculate rate standard error 100 x SQRT (1/D+1/B)
         # formula from health department guidebook for SE of rate: 1.96*sqrt((rate/population)*rate_denominator)
         se_rate_a = 100*sqrt(1/(deaths+1)/births),
         se_rate = sqrt((rates/births)*deaths))

# formual for infant mortality rate: (num deaths / total live births) * 1000
# formula from health department guidebook for SE of rate: 1.96*sqrt((rate/population)*rate_denominator)

infant <- left_join(rates, counts, by = c('year', 'geo_area'))