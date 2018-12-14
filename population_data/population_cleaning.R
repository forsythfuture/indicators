library(tidyverse)

# create vector of yearly NC population files
raw_data_files <- list.files('population_data/raw_data', 
                             pattern = 'nc_county', # only get NC populatino files
                             full.names = TRUE)

## import each yearly file, and year to dataset, bind with previous years

# initialize dataframe to store results
counties <- data.frame()

for (file in raw_data_files) {
  
  # extract the year number from file name
  # the year is the two digit year number at the end of the file name
  single_year <- str_extract(file, '[0-9][0-9]')
  
  # convert year number to integer and add 2000, so the final result is in the format YYYY
  single_year <- as.integer(single_year) + 2000
  
  # import dataset
  counties <- read_delim(file, "\t", escape_double = FALSE, trim_ws = TRUE) %>%
    # add year
    mutate(year = single_year) %>%
    # notes column is NA for rows that contain complete information
    filter(is.na(.$Notes)) %>%
    bind_rows(counties, .) 
  
}

# rename county column and drop county code so that counties can be bound to US popualtion dataset
counties <- counties %>%
  rename(geo_description = County) %>%
  select(-Notes, -`County Code`, -`Gender Code`, -`Ethnicity Code`, -`Race Code`, -`Age Group Code`) %>%
  mutate(geo_area = 'county') %>%
  # reformat county names so that they match style of names in other datasets
  mutate(geo_description = str_replace_all(geo_description, ' County, NC', ''))

# create function to import US and NC data
# data shares same format, so common function is possible
population_aggregate <- function(filename, geographic_description, geographic_area) {

  # import population data
  df <- read_delim(filename, 
                   "\t", escape_double = FALSE, trim_ws = TRUE) %>%
    # add column for geography
    mutate(geo_description = !!geographic_description) %>%
    # notes column is NA for rows that contain complete information
    filter(is.na(.$Notes)) %>%
    # rename column so it matches counties dataset
    rename(year = `Yearly July 1st Estimates`) %>%
    select(-`Yearly July 1st Estimates Code`,-Notes, -`Gender Code`, 
           -`Ethnicity Code`, -`Race Code`, -`Age Group Code`) %>%
    # add column signifying the type of geographic area
    mutate(geo_area = !!geographic_area)
  
  return(df)
  
}

# import US and NC datasets, and combine them
us <- population_aggregate('population_data/raw_data/us_population_06_17.txt',
                           'United States', 'US')

nc <- population_aggregate('population_data/raw_data/nc_population_06_17.txt',
                           'North Carolina', 'state')

population <- bind_rows(us, nc) %>%
  bind_rows(., counties)
  
# rename column names so that there are no two word column names
colnames(population) <- c('gender', 'ethnicity', 'race', 'age', 'year', 'population', 'geo_description', 'geo_area')

# reorder columns
population <- select(population, year, geo_area, geo_description, everything())

# hispanic / latino is in the ethnicity column, while white and AA are in the race column
# find the total hispanic / latino population by summing the ethnicity column by 
# hispanic / latino or not hispanic / latino
hispanic <- population %>%
  group_by(year, geo_description, gender, age, ethnicity) %>%
  summarize(population = sum(population)) %>%
  ungroup() %>%
  filter(ethnicity == 'Hispanic or Latino') %>%
  mutate(ethnicity = 'Hispanic/Latino') %>%
  rename(race = ethnicity)

# add hispanic / latino population to primary dataset
population <- population %>%
  # remove hispanic / latino, since we using the aggregate totals
  filter(ethnicity != 'Hispanic or Latino') %>%
  # remove ethnicity, since hispanic / latino is in the race column
  select(-ethnicity) %>%
  # add dataset with hispanic / latino totals
  bind_rows(hispanic) %>%
  # sort
  arrange(year, geo_area, geo_description, gender, race, age) %>%
  # rename race descriptions for consistency with other datasets
  mutate(race = str_replace_all(race, 'White', 'White, non-Hispanic'),
         race = str_replace_all(race, 'Black or African American', 'African American'))

#write_csv(population, 'population_data/population_cleaned.csv')