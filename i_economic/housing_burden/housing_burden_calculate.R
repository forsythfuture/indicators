################################################################################
#
# Description of dataset variables:
#   AGEP: age category; number represents top age in age bracket
#         for example: 44 represents people 25 to 44
#                      150 represents top age bracket: 65 and over
#   RAC1P: Racial category:
#           1 = White
#           2 = African American
#           3 = Hispanic
#           4 = other
#   housing_status: whether the person is an owner or renter
#   percentge: the percentage of houshold income that goes to rent or homeownership
#
######################################################################################

library(tidyverse)

# import raw housing burden file
housing_burden <- readRDS('i_economic/housing_burden/housing_burden.rds')

colnames(housing_burden) <- c('age', 'race', 'weight', 'tenure', 'pct_housing', 'year', 'geography')

housing_burden <- housing_burden %>%
  mutate(race = ifelse(race=='1','White, non-Hispanic', 
                     ifelse(race=='2','African American',
                            ifelse(race=='3','Hispanic','Other'))))%>%
  mutate(age = ifelse(age=='24', '24 years and under',
                      ifelse(age=='44', '25 to 44 years',
                             ifelse(age=='64', '45 to 64', '65 years and over'))))
  
housing_burden <- housing_burden %>%
  # extend the number of rows based on the weight
  uncount(weight) %>%
  mutate(pct_housing = ifelse(pct_housing > 30, 'yes', 'no'))

total_trend <- housing_burden %>%
  group_by(geography, year)%>%
  summarise(estimate = sum(pct_housing == "yes")/n()) %>%
  mutate(type = 'Comparison Community',
         subtype = 'total')

tenure_trend <- housing_burden%>%
  group_by(geography, year, tenure)%>%
  summarise(estimate = sum(pct_housing == "yes")/n())

race <- housing_burden %>%
  group_by(geography, year, race)%>%
  summarise(estimate = sum(pct_housing == "yes")/n()) %>%
  filter(race != 'Other') %>%
  rename(subtype = race) %>%
  mutate(type = 'Race / Ethnicity')

age <- housing_burden %>%
  group_by(geography, year, age)%>%
  summarise(estimate = sum(pct_housing == "yes")/n()) %>%
  rename(subtype = age) %>%
  mutate(type = 'Age')

housing_burden <- bind_rows(total_trend, race) %>%
  bind_rows(., age)












