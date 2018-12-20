################################################################################
#
# Description of dataset variables:
#   AGEP: age category; number represents top age in age bracket
#         for example: 44 represents people 25 to 44
#                      15 represents top age bracket: 65 and over
#   RAC1P: Racial category:
#           1 = White
#           2 = African American
#           3 = Hispanic
#           4 = other
#   housing_status: whether the person is an owner or renter
#   percentge: the percentage of houshold income that goes to rent or homeownership
#
######################################################################################

# import raw housing burden file
housing_burden <- readRDS('i_economic/housing_burden/housing_burden.rds')

colnames(housing_burden) <- c('age', 'race', 'weight', 'tenure', 'pct_housing', 'year', 'geography')

housing_burden <- housing_burden %>%
  uncount(weight) %>%
  mutate(pct_housing = ifelse(pct_housing > 30, 1, 0))

total_trend <- housing_burden %>%
  group_by(geography, year)%>%
  summarise(estimate = sum(pct_housing == "1")/n())

#tenure_trend <- housing_burden %>%
 # group_by(geography, year, tenure)%>%
  # summarise(estimate = sum(pct_housing == "1")/n())












