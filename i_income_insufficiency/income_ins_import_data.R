library(tidyverse)

### consumer expenditure survey ###

#######################################################################################################

clean_ces <- function(data_file, header_file) {
  
  # import data
  df <- read_delim(data_file,
                      delim = '\t') %>%
    # convert from wide (each column is a different year) 
    # to long (each row is a different year and expense)
    gather("Year", 'Expense', `Annual 2006`:`Annual 2017`) %>%
    # remove word 'Annual' from year column
    mutate(Year = str_replace_all(Year, 'Annual ', '')) %>%
    # trim whitespace in all columns
    # need to trim so values in this column can match values in dataframe containing estimates
    mutate_all(funs(str_trim(., side = 'both'))) %>%
    # convert years and expense estimtae to numeric
    mutate_at(c('Expense', 'Year'), as.numeric)
  
  # import headers
  df_headers <- read_delim(header_file,
                              delim = '\t',
                              col_names = FALSE) %>%
    # only keep headers
    filter(!str_detect(X1, '^CXU'),
           # remove rows that jsut say 'Series ID'
           X1 != 'Series Id') %>%
    # separate columsn at ':', this puts header title and header value in different columns
    separate(X1, into = c('titles', 'values'), sep = ': ', extra = 'merge', remove = TRUE) %>%
    # add column that adds an ID number to each series group
    # needed to spread columns
    mutate(id = rep(1:(nrow(.)/6), each = 6)) %>%
    # convert from long to wide where each header title is in a different row
    spread(titles, values) %>%
    # remove unneeded values
    select(-id, -Category) %>%
    # rename to match column name in dataframe containing data
    # we will merge on this column, so names need to be the same
    rename(`Series ID` = `Series Id`) %>%
    # trim whitespace in all columns
    # need to trim so values in this column can match values in dataframe containing estimates
    mutate_all(funs(str_trim(., side = 'both')))
  
  # combine header descriptions to dataframe containing expenses dat
  df <- left_join(df, df_headers, by = 'Series ID')
  
  ### adjust expenses of number of people in house by region (south)
  
  ###################################################################################################
  # The data shows average national expenses by type, averge region expenses, 
  # and average expenses by number of poepl in household.
  # But, there is no data for expenses by average number of people in household and region.
  # Therefore, we will adjust household expenses by the ration of the south to the national average
  # in each expense category
  ##################################################################################################
  
  # create ratio of south to the rest of the United States by splitting entire US and South into different datasets,
  # then merging; this puts each region's estimate on the same row
  
  us <- df %>%
    filter(Characteristics == 'All Consumer Units') %>%
    select(Item, Year, Expense)
  
  south <- df %>%
    filter(Characteristics == 'Region of residence: south') %>%
    select(Item, Year, Expense)
  
  # merge (must join by item because series ID is different depending on region and num of people in houshold)
  ratio <- left_join(us, south, by = c('Item', 'Year')) %>%
    # calculate ratio by dividing South estimate by US estimate
    mutate(region_ratio = Expense.y / Expense.x) %>%
    # drop column not needed to join ratio to dataset containing estimate
    select(Item, Year, region_ratio)
  
  # remove region rows from dataset contining expenses, because we only needed
  # regions to calculate ratio
  df <- df %>%
    filter(!(Characteristics == 'All Consumer Units' |
               Characteristics == 'Region of residence: south')) %>%
    # add region adjusted ratios
    left_join(ratio, by = c('Item', 'Year')) %>%
    # multiply expense by region ratio, round to nearest whole number
    mutate(Expense = round( Expense*region_ratio, 0 )) %>%
    # ratios are no longer needed
    select(-region_ratio)
  
  return(df)
}

######################################################################################

# create path to folder with data (to shorten file names that have to be entered)

data_path <- 'i_income_insufficiency/data/'

### health insurance for households
# health insurance for poeple over 65 is in a different table
health_household <- clean_ces(paste0(data_path, 'health/health_ins_household_raw_data.txt'),
                              paste0(data_path, 'health/health_ins_household_headers.txt'))

### transportation
trans <- clean_ces(paste0(data_path, 'transportation/transportation_raw_data.txt'),
                   paste0(data_path, 'transportation/transportation_headers.txt'))


