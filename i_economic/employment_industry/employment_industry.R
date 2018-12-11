library(tidyverse)

# data for yearly annual employment by industry comes from: https://www.bls.gov/cew/datatoc.htm
# data disctionary is here: https://data.bls.gov/cew/doc/layouts/csv_annual_layout.htm

# pound signs are not present in data
industry_titles <- c('Total, all industries',
                     'Management of companies and enterprises',
                     'Finance and insurance',
                     'Professional and technical services',
                     'Manufacturing',
                     #'Wholesale trade', # 2008 does not have any data, so skip
                     'Educational services',
                     'Health care and social assistance',
                     # 'Transportation and warehousing', # dropped from 7000 jobs in 2008 to 600 jobs in 2017; so removed 
                     'Public administration',
                     'Construction',
                     'Administrative and support services',
                     'Retail trade',
                     'Other services',
                     'Accommodation and food services',
                     #'Utilities', # lists no employment
                     'Natural resources and mining',
                     'Information',
                     'Real estate',
                     'Arts, entertainment, and recreation')

# read in both years data
emp <- read_csv('i_economic/employment_industry/forsyth_jobs_industry_08.csv') %>%
  bind_rows(read_csv('i_economic/employment_industry/forsyth_jobs_industry_17.csv')) %>%
  # some of the industry descriptions have 'NAICS' and numbers; remove these
  mutate(industry_title = str_trim( str_replace_all(industry_title, 'NAICS |\\d|[-]', ""), side = 'both')) %>%
  select(year, area_title:industry_title,
         annual_avg_emplvl, # average annual monthly employment
         total_annual_wages, # total annual wages
         avg_annual_pay # average annual pay
         ) %>%
  filter(industry_title %in% industry_titles) %>%
  # dataset has rows for public and private employment by industry
  # group public and private, and sum for total employment by industry
  group_by(year, industry_title) %>%
  summarize(industry_size = sum(annual_avg_emplvl, na.rm = TRUE),
            wages_sum = sum(total_annual_wages, na.rm = TRUE)) %>%
  mutate(average_wages = round( wages_sum / industry_size, 0 )) %>%
  select(-wages_sum) %>%
  unique()

# create dataset for 2017 and 2008
seventeen <- emp %>%
  filter(year == 2017) %>%
  # order by industry size so we can add a column representing whether industry is in the top six
  arrange(desc(industry_size))

# add column representing whether industry is one of the six largest
seventeen$largest_six <- c('no', rep('yes', 6), rep('no', nrow(seventeen)-7))

# pull out total employment, so we can calculate each industry's share of jobs
total_employment <- seventeen$industry_size[seventeen$industry_title == 'Total, all industries']

# add column of each industry's share of total employment
seventeen$share_employment <- round( seventeen$industry_size / total_employment, 2 )

# rearrange rows by industry title, so 2017 and 2008 match in ordering
seventeen <- arrange(seventeen, industry_title)

eight <- emp %>%
  filter(year == 2008) %>%
  arrange(industry_title)

# write out industry size for 2017 dataset
#seventeen %>%
#  arrange(desc(industry_size)) %>%
#  write_csv('i_economic/employment_industry/cleaned_data/emp_industry_17.csv')

# create column that show raw numbers of employemnt growth between 2008 and 2017
seventeen$emp_growth <- seventeen$industry_size - eight$industry_size
