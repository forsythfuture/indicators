library(tidyverse)


# create list of raw data files, including path to directory
directory <- 'i_demographics/data/raw_data/'
# create list of raw data files, including path to directory
raw_data_files <- paste0(directory, list.files(directory))

## import each yearly file, and year to dataset, bind with previous years

# initialize dataframe to store results
df <- data.frame()

for (file in raw_data_files) {
  
  # extract the year number from file name
  single_year <- str_extract(file, '[0-9][0-9]')
  
  # convert year number to integer and add 2000, so the final result is in the format YYYY
  single_year <- as.integer(single_year) + 2000
  
  # import dataset
  df <- read_delim(file, "\t", escape_double = FALSE, trim_ws = TRUE) %>%
    # add year
    mutate(year = single_year) %>%
    bind_rows(df, .)
  
}

# remove unneeded variables (primarily code variables)
df <- select(df, -`County Code`, -`Gender Code`, -`Ethnicity Code`, -`Race Code`, -`Age Group Code`)

# rename column names so that there are no two word column names
colnames(df) <- c('notes', 'county', 'gender', 'ethnicity', 'race', 'age', 'population', 'year')

# write out dataset
#write_csv(df, 'i_demographics/data/demographic_data.csv')

#df %>% filter(Notes == 'Total') %>%
  #filter(!is.na(father), !is.na(father)) %>%
#  filter_at(vars(County:`Age Group Code`), all_vars(is.na(.)))
