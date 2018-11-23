library(tidyverse)

# import data
grad_rates <- read_csv('i_education/grad_rates/grad_rates.csv') %>%
  # data currently only has columns for total number of students and percentage grads
  # create columns for number of grads and number of non-grads
  mutate(grads = round(total*percent, 0),
         non_grads = total-grads)
# data has economically disadvantaged rates, but not rates for non-economically disadvantaged
# create this category by using total and economically disadvantages
eds <- grad_rates %>%
  filter(group == 'EDS')
all <- grad_rates %>%
  filter(group == 'ALL' & geo_area != 'school')


# create function to calculate non-eds values by subtracting total from eds
non_eds_values <- function(col_name) {
  return(all[[col_name]] - eds[[col_name]])
}

# function to calculate standard error of binomial
se_binom <- function(p, n) {
  return(sqrt(p*(1-p)/n))
}

# create dataframe for non_eds
non_eds <- data.frame(year = all$year,
                      district = all$district,
                      group = 'NON_EDS',
                      total = non_eds_values('total'),
                      grads = non_eds_values('grads'),
                      non_grads = non_eds_values('non_grads')) %>%
  mutate(percent = grads /total,
         geo_area = ifelse(district == 'NC', 'state', 'district'))

# bind non-EDS to regular dataframe
grad_rates <- grad_rates %>%
  bind_rows(non_eds) %>%
  # create combined column of year, county, and ethnicity for model 
  mutate(level = paste(year, district, group, sep = ' '),
         # calcualte standard error of binomial
         se = se_binom(percent, total),
         moe = se * 1.96,
         cv = (se / percent) * 100,
         # change labeling of counties and state
         # change NC ro North Carolina and add 'County, NC' to counties
         district = ifelse(geo_area == 'state', 'North Carolina',
                          ifelse(geo_area == 'district', paste0(district, ' County, NC'),
                                ifelse(geo_area == 'school', paste0(district, ' High School'), 
                                       district)))) %>%
  mutate_at(vars(se, moe, cv, percent), funs(round(., 2))) %>%
  # sort by district, year, and demographic
  arrange(geo_area, district, year, group) %>%
  # rename to match variable names in shiny app
  rename(geo_description = district, estimate = percent, subtype = group) %>%
  # replace demographic abbreviations will better descriptions
  mutate(subtype = str_replace_all(subtype, '^ALL', 'Total'),
         subtype = str_replace_all(subtype, '^B', 'African American'),
         subtype = str_replace_all(subtype, '^EDS', 'Economically Disadvantages'),
         subtype = str_replace_all(subtype, '^F', 'Female'),
         subtype = str_replace_all(subtype, '^H', 'Hispanic/Latino'),
         subtype = str_replace_all(subtype, '^M', 'Male'),
         subtype = str_replace_all(subtype, '^NON_EDS', 'Not Economically Disadvantaged'),
         subtype = str_replace_all(subtype, '^W', 'White, non-Hispanic'),
         # create column for demographic type
         type = ifelse(str_detect(subtype, '^M|^F'), 'Gender',
                       ifelse(str_detect(subtype, '^A|^W|^H'), 'Race/Ethnicity',
                              ifelse(str_detect(subtype, '^E|^N'), 'Economic Status',
                                     'Total'))))


# explort cleaned dataset
#write_csv(grad_rates, 'i_education/grad_rates/grad_rates_cleaned.csv'