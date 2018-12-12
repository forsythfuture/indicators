library(tidyverse)

# import file with inflation adjustment function
source('functions/acs/acs_misc_functions.R')

# data for yearly annual employment by industry comes from: https://www.bls.gov/cew/datatoc.htm
# data disctionary is here: https://data.bls.gov/cew/doc/layouts/csv_annual_layout.htm

# industry codes of industries to use
industry_codes <- c('10', '101', '102', '1012', '1013', '1022', '1023', '54', '71', '72',
                    '1027', '1028', '44-45', '55', '61', '62', '48-49', '53', '56')

# read in both years data
emp <- read_csv('i_economic/employment_industry/forsyth_jobs_industry_08.csv') %>%
  bind_rows(read_csv('i_economic/employment_industry/forsyth_jobs_industry_17.csv')) %>%
  # some of the industry descriptions have 'NAICS' and numbers; remove these
  mutate(industry_title = str_trim( str_replace_all(industry_title, 'NAICS |\\d|[-]', ""), side = 'both')) %>%
  select(year, industry_code, area_title:industry_title,
         annual_avg_emplvl, # average annual monthly employment
         total_annual_wages, # total annual wages
         avg_annual_pay # average annual pay
         ) %>%
  filter(industry_code %in% industry_codes,
         # total covered includes all private and government
         # remove these items, so we do not double count them when we sum government and private sectors
         own_title != 'Total Covered') %>%
  # group public and private, and sum for total employment by industry
  group_by(year, industry_code, industry_title) %>%
  summarize(industry_size = sum(annual_avg_emplvl, na.rm = TRUE),
            wages_sum = sum(total_annual_wages, na.rm = TRUE)) %>%
  # find average wages by dividing total wages by industry size
  mutate(average_wages = round( wages_sum / industry_size, 0 )) %>%
  select(-wages_sum) %>%
  # make names more descriptive
  mutate(industry_title = str_replace_all(industry_title, 'Goodsproducing', 'Total, all goods producing'),
         industry_title = str_replace_all(industry_title, 'Serviceproviding', 'Total, all service sector'),
         industry_title = str_replace_all(industry_title, 'Administrative and waste services', 'Administrative and support'),
         industry_title = str_replace_all(industry_title, 'Professional and technical services', 'Professional services'),
         industry_title = str_replace_all(industry_title, 'Real estate and rental and leasing', 'Real estate')) %>%
  unique()

# adjust for inflation by converting 2008 dollars to 2017 dollars
emp <- ff_inflation_adjust(emp, average_wages, se_col = NULL, year_adjust = 2017, error = FALSE) %>%
  select(-average_wages) %>%
  rename(average_wages = estimate_adj)

# create dataset for 2017 and 2008
seventeen <- emp %>%
  filter(year == 2017) %>%
  # order by industry size so we can add a column representing whether industry is in the top six
  arrange(desc(industry_size))

# add column representing whether industry is one of the six largest
seventeen$largest_six <- c('no', 'no', 'yes', 'no', rep('yes', 5), rep('no', nrow(seventeen)-9))

# pull out total employment, so we can calculate each industry's share of jobs
total_employment <- seventeen$industry_size[seventeen$industry_title == 'Total, all industries']

# add column of each industry's share of total employment
seventeen$share_employment <- round( seventeen$industry_size / total_employment, 2 )

# rearrange rows by industry title, so 2017 and 2008 match in ordering
seventeen <- arrange(seventeen, industry_title)

eight <- emp %>%
  filter(year == 2008) %>%
  arrange(industry_title)

# create function for calculating growth of metric (wage or employment)
growth <- function(old, new) round((new - old) / old, 2)

# create column that show raw numbers and percentiles of employemnt growth between 2008 and 2017
seventeen$emp_growth_raw <- seventeen$industry_size - eight$industry_size
seventeen$emp_growth_perc <- growth(eight$industry_size, seventeen$industry_size)
seventeen$wage_growth <- growth(eight$average_wages, seventeen$average_wages)

seventeen %>%
  ungroup() %>%
  arrange(desc(industry_size)) %>%
  select(-industry_code) %>%
  write_csv('i_economic/employment_industry/cleaned_data/emp_industry_17.csv')
