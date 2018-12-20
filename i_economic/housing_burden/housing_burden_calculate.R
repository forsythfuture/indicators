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
  # extend the number of rows based on the weight
  uncount(weight) %>%
  mutate(pct_housing = ifelse(pct_housing > 30, 'yes', 'no'))

total_trend <- housing_burden %>%
  group_by(geography, year)%>%
  summarise(estimate = sum(pct_housing == "yes")/n())

tenure_trend <- housing_burden%>%
  group_by(geography, year, tenure)%>%
  summarise(estimate = sum(pct_housing == "yes")/n())


#tenure <- housing_burden %>%
 #group_by(geography, year, pct_housing, tenure)%>%
  #summarise(count=n())

#tenure_spread <- spread(tenure, key = pct_housing, value = count)
  
#tenure_trend_v2 <- tenure_spread %>%
 # mutate(estimate = yes/(no + yes))
  












