library(tidyverse)

# pound signs are not present in data
industry_titles <- c('Management of companies and enterprises',
                     'Finance and insurance',
                     'Professional and technical services',
                     'Manufacturing',
                     'Wholesale trade',
                     'Educational services',
                     'Health care and social assistance',
                     'Transportation and warehousing',
                     'Public administration',
                     'Construction',
                     'Administrative and support services',
                     'Retail trade',
                     'Other services',
                     'Accommodation and food services')

# read in both years data
empA <- read_csv('i_economic/employment_industry/forsyth_jobs_industry_08.csv') %>%
  bind_rows(read_csv('i_economic/employment_industry/forsyth_jobs_industry_17.csv')) %>%
  # some of the industry descriptions have 'NAICS' and numbers; remove these
  mutate(industry_title = str_trim( str_replace_all(industry_title, 'NAICS |\\d|[-]', ""), side = 'both')) %>%
  select(year, area_title:industry_title,
         annual_avg_emplvl, # average annual monthly employment
         avg_annual_pay # average annual pay
         ) %>%
  filter(industry_title %in% industry_titles) %>%
  # dataset has rows for public and private employment by industry
  # group public and private, and sum for total employment by industry
  group_by(c('year', 'industry_title')) %>%
  summarize()
  
  unique()

emp %>% filter(year == 2017)
emp %>% filter(str_detect(industry_title, 'Management'))
# vector of the columns that are needed