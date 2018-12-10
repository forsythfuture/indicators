library(tidyverse)

# pound signs are not present in data
industry_titles <- c('Management of companies and enterprises',
                     'Finance and insurance',
                     'Professional scientific and technical services',
                     'Professional and technical services',
                     'Manufacturing',
                     'Wholesale trade',
                     'Educational services',
                     'Health care and social assistance',
                     'Transportation and warehousing',
                     'Public administration',
                     'Construction',
                     'Administrative and support and waste management and remediation services',
                     'Administrative and waste services',
                     'Retail trade',
                     'Other services (except public administration)',#
                     'Accommodation and food services')

# read in both years data
emp <- read_csv('i_economic/employment_industry/forsyth_jobs_industry_08.csv') %>%
  bind_rows(read_csv('i_economic/employment_industry/forsyth_jobs_industry_17.csv')) %>%
  # some of the industry descriptions have 'NAICS' and numbers; remove these
  mutate(industry_title = str_replace_all(industry_title, 'NAICS |[0-9]|[0-9] |^[-]', "")) %>%
  select(year, area_title:industry_title,
         annual_avg_emplvl, # average annual monthly employment
         avg_annual_pay # average annual pay
         ) %>%
  #filter(industry_title %in% industry_titles) %>%
  unique()

emp %>% filter(str_detect(industry_title, 'Management'))
# vector of the columns that are needed