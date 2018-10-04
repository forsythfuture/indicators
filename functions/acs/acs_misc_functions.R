################################################################
# This file contains functions that unzip and clean ACS files.
################################################################

library(tidyverse)


ff_unzip_files <- function(file_path, output_dir) {
  
  # This function takes a zipped file of ACS data, unzips it,
  # and only keeps the data files
  
  # Input:
    # file_path: path to zip file
    # output_dir: directory for outpt
  
  # unzip files
  unzip(file_path, exdir = output_dir)
  
  # we do not want text files and metadata files; metadata is in data file
  # delete these files
  # need to paste directory to file names
  files_remove <- paste0(output_dir, '/', list.files(path = output_dir, pattern = 'metadata|txt'))
  file.remove(files_remove)
  
}

ff_clean_acs <- function(file_name, year) {
  
  # This function takes as input an ACS file as downloaded from AFF,
  # adds descriptions, and converts to long form
  
  # Input:
  #   file_name: The file name to a csv file downloaded from AFF
  
  df <- read_csv(file_name)
  
  # The data is in wide-form where each item is a different column
  # we want to convert to long form where there are columns for estimate and and moe
  # and each row is a different item
  # The first row contains the column description
  # It includes the word 'Estimate;' for estimates and 'Margin of Error;' for moe
  # To convert to long form create two datasets, one for estimates and one for MOE
  # the datasets will then be merged
  
  df <- df %>%
    # delete unneeded columns
    select(-`GEO.id`, -`GEO.id2`) %>%
    # change column name
    rename(geo_description = `GEO.display-label`)
  
  # first row (under column header) contains metadata (description), so extract and place into dataframe
  # also include column headers because the headers will be used to merge metadata with data
  metadata <- t(df[1,]) %>%
    as.data.frame() %>%
    # column headers are the row names, convert to a column so that descriptions can be merged with data set
    mutate(label = row.names(.)) %>%
    # rename column
    rename(description = V1) %>%
    # filter out first row, which is the column description
    .[2: nrow(.),]
  
  # since the descriptions are extracted, we can delete the row containing them
  df <- df[2:nrow(df),]

  # convert from wide to long, where each estimate and moe is on a different row
  df <- df %>% 
    gather(label, estimate, -geo_description) %>%
    # combine descriptions, which are in the metadata object, with the data set containing data
    left_join(metadata, by = 'label')
      
  # create two different datasets, one for MOE and one for estimates
  # these two data sets will be merged so that there is one data set with the estimate and moe as seperate columns
  moe <- df %>%
    filter(str_detect(description, 'Margin of Error; ')) %>%
    # rename moe estimate column so it has a different name than the column with estimate values
    rename(moe = estimate)
  df <- df %>%
    filter(str_detect(description, 'Estimate; ')) 
  
  # estimate and moe data sets should have the same number of rows,
  # and the rows should match by row number
  # for example the fourth row for moe data set should contain the moe
  # for the fourth row of the estimate data set
  
  # first ensure each data set has the same number of rows, if not stop and throw error message
  if (nrow(df) != nrow(moe)) {
    stop('The estimate and moe data sets do not contain the same number of rows.')
  }
  
  # add moe values to data frame
  df$moe <- moe$moe
  # add year as a column
  df$year <- year
  

  df <- df %>%
    # convert estimate, and moe to numbers, description from factor to character
    mutate(estimate = as.numeric(estimate),
           moe = as.numeric(moe),
           description = as.character(description)) %>%
    # the ACS default is a 90% MOE, convert to 95% margin of error
    # reference: A compass for understanding and using ACS data, October 2008, A-12 
    mutate(moe = round((1.96/1.645) * moe, 2)) %>%
    # calcualte standard error and cv
    # reference: A compass for understanding and using American Community Survey Data, Oct. 2008, A-12
    mutate(se = round(moe / 1.96, 2),
           cv = round((se / estimate)*100, 2)) %>%
    # counties have the county name and then 'County, North Carolina'
    # convert 'North Carolina' to NC
    mutate(geo_description = str_replace(geo_description, 'County, North Carolina', 'County, NC')) %>%
    # change order of columns
    select(geo_description, label, year, description, estimate, moe, se, cv)
    
  return(df)
  
}

ff_import_acs <- function(zip_file, raw_data_path, years) {
  
  # This file takes as input a .zip file of AFF downloaded data
  # and outputs a sinlge cleaned data set of all the files in the .zip file
  
  # input:
  #  zip_file: the file name and full path to the zip file
  #  raw_data_path: The folder that the raw data should be copied to
  #  years: vector of years represented in the data
  
  # unzip files
  # they will be temporarily stored in the same folder as the zip files
  ff_unzip_files(zip_file, raw_data_path)
  
  # list of files in zip file (each file represents a year of data)
  data_files <- paste0(raw_data_path, '/', dir(path=raw_data_path, pattern=".csv"))
  
  ### iterate through each file and year, extract data, and bind to previous year
  
  # initialize dataframe
  df_full <- data.frame()
  
  for (i in seq_along(data_files)) {
    
    # import one year of data
    df <- ff_clean_acs(data_files[i], years[i])
    
    # bind to previous year
    df_full <- df_full %>%
      bind_rows(df)
  }
  
  return(df_full)
  
}

ff_cv_color <- function(df) {
  
  # this function takes as input the cv column name as a string
  # it mutates it by adding color to cv numbers for a kable table
  
  # No color is added if the cv is less than 12
  # Blue is added for CVs between 12 and 30
  # Red is added for CVs over 30
  
  df %>%
    # must convert missing values to numbers because cell_spec cannot take missing values
    # since missing values signify high cv, convert to 1000
    # this number will be converted back to NA after creating colors
    mutate(cv = ifelse(is.na(.$cv), 100, .$cv)) %>%
    # change color  
    mutate(cv = cell_spec(cv, color = ifelse(cv > 12 & cv <= 30, "blue",
                                             ifelse(cv > 30, 'red', 'black'))))
}


ff_disparities <- function(df, comparison_column, comparison_demographics, comparisons, years, geo_areas) {
  
  # input:
  #   df: a dataframe where there is a column called ethnicity with values of either:
  #         'African American', 'Hispanic/Latino', or 'White, non-Hispanic'
  #   comparison_column: the column, as a string, that contains the description of the demographic to be compared
  #                           example: c('African american', 'White, non-hispanic')
  #   comparisons: a list of vectors signifying which comparisons to make
  #                 each element in the list is a different comparison
  #                 for example: list(c(1,3), c(3,2)) says that we should compare the first string in comparison_demographic
  #                              with the third string, and compare the third string with the second
  #   years: the years in the dataset
  #   geo_areas: the geographic areas in the dataset
  
  ## create a seperate dataframe for each comparison demographic and add dataframe to list
  
  # initialize list
  comparison_df <- list()
  
  # iterate through each comparison demographic, filter for that demographic,
  # and adding dataframe to a list with the demographic ias the list element's name
  for (demo in comparison_demographics) {
    comparison_df[[demo]] <- df[df[comparison_column] == demo,]
  }
  
  # each dataframe in the list should have the same number of rows
  # there is a problem if there are a different number of rows
  # check to ensure each dataframe has the same number of rows
  # process: 
  #   sapply: find the number of rows for each dataframe
  #   unique: only keep unique values for the number of rows
  #           if all dataframes have the same number of rows then there will only be one unique value
  #           if there is more than one unique value of row numbers, stop function and print message
  if (length(unique(sapply(comparison_df, nrow))) != 1) {
    
    stop('There is a problem. The demographic dataframes do not have the same number of rows')
    
  }
    
  ## calculate ratios of each demographic that is being compared and place ratios in list
  
  # initialize list
  demographic_ratios <- list()
  
  for (i in seq_along(comparisons)) {
    
    # identify which dataframe in comparison_df will be the numerator and which will be the denominator
    numerator <- comparisons[[i]][1]
    denominator <- comparisons[[i]][2]
    
    # create a description of the ratio being generated
    # this will be added as a column to the ratio dataframe
    ratio_description <- paste0(comparison_demographics[numerator], ' to ',
                                comparison_demographics[denominator], ' ratio')
    
    
    # calculate ratio and add to list                            
    demographic_ratios[[i]] <- ff_acs_ratios(comparison_df[[numerator]][['estimate']], 
                                             comparison_df[[numerator]][['moe']],
                                             comparison_df[[denominator]][['estimate']], 
                                             comparison_df[[denominator]][['moe']]) %>%
      #add years, repeat each year once for each geographic area
      mutate(year = rep(years, each=length(geo_areas)),
             # add geographic areas; add each geographic area once for each year
             geo_description = rep(geo_areas, times=length(years)),
             # add description of comparison
             description = ratio_description)
    
  }
  
  # combine all three ratio dataframes into one dataframe
  
  demographic_ratios <- bind_rows(demographic_ratios) %>%
    # reorder columns
    select(year, description, geo_description, everything())
  
  return(demographic_ratios)
}


ff_ratios_gender <- function(df, years, geo_areas) {
  
  # input:
  #   df: a dataframe where there is a column called gender with values of either 'Male', or 'Female'
  #   years: the years in the dataset
  #   geo_areas: the geographic areas in the dataset
  
  # create different datasets for each gender
  # this is needed to calculate ratios by gender
  gender_male <- filter(df, gender == 'Male')
  gender_female <- filter(df, gender == 'Female')
  
  # each dataset must have the same number of rows, if they do not there is a problem
  # check to see if all the datasets do not have the same number of rows
  if (!(nrow(gender_male) == nrow(gender_female))) {
    
    print('!!!!! There is a problem. All the gender comparison data sets do not have the same number of rows!!!!')
    
  }
  
  # calculate ratio and moe of each racial comparison
  gender_ratios <- ff_acs_ratios(gender_male$estimate, gender_male$moe, 
                                 gender_female$estimate, gender_female$moe) %>%
    #add years, repeat each year once for each geographic area
    mutate(year = rep(years, each=length(geo_areas)),
           # add geographic areas; add each geographic area once for each year
           geo_description = rep(geo_areas, times=length(years)),
           # add description of comparison
           description = 'Male to female ratio')
  
  return(gender_ratios)
  
}

ff_data_dt <- function(df, col_names, for_tableau=FALSE) {
  
  # Input:
  #   df: a dataframe of raw data that you want convert to a DT datatable
  #   col_names: column names for the datatable
  #   for_tableau: whether this table is for tableau output
  #
  # To color cv values, the cv column must be names 'cv'
  #
  # Note: Do not sue this to create a table of z-scores; use ff_acs_zscore_dt
  
  if (for_tableau == FALSE) {
  
    datatable(df, filter='top', extensions='Buttons', rownames = FALSE,
              colnames = col_names,
              options = list(scrollX = TRUE, scrollY = TRUE, dom = 'Bfrtip')) %>%
      # color cv numbers based on value, only if column named 'cv' exists
        formatStyle('cv', color = styleInterval(c(12, 30), c('black', 'blue', 'red')))
    
  } else {
    
    # if the table is for tableau, we need to add additional rows that represent Forsyth County totals,
    # but change type from Comparison Community to Total
    
    df %>%
      # filter for rows with Forsyth County as the county, and where type starts with Comparison
      filter(str_detect(geo_description, '^Forsyth'),
             str_detect(type, '^Comparison')) %>%
      # change type columns from Comparison to Total
      mutate(type = 'Total') %>%
      # bind these rows to the original dataframe
      bind_rows(df)  %>%
      # create datatable
      datatable(filter='top', extensions='Buttons', rownames = FALSE,
                colnames = col_names,
                options = list(scrollX = TRUE, scrollY = TRUE, dom = 'Bfrtip'))
    
  }
}


ff_inflation_adjust <- function(df, wages_col, se_col, year_adjust) {
  
  library(xts)
  library(lubridate)
  #########################################################################
  # This function takes as input a dataframe that contains dollar amounts
  # that must be adjusted for inflation and returns the same dataframe, 
  # but with an additional column for inflation ajusted dollar amounts
  #
  # Input:
  #   df: name of dataframe that contains dollar amounts
  #   wages_col: column name of column in dataframe containing dollar amounts
  #              entered as object name (no quotes), not string 
  #              (example: as wages and not "wages")
  #   se_col: column that contains standard error
  #   year_adjust: adjust all dollar amounts to this year
  #
  # Output:
  #   The same dataframe, but with an additional column called 'estimate_adj'
  #
  #   !!!! Important Note: the column that contains years must be called 'year'
  #
  # Reference: US Census Bureau, A Compass for Understanding and Using ACS Data, 
  #             October 2008, A-22 
  #
  #########################################################################
  
  wages_col <- enquo(wages_col)
  se_col <- enquo(se_col)
  
  # import CPI All items index data
  monthly_cpi <- read.table("http://research.stlouisfed.org/fred2/data/CPIAUCSL.txt",
                            skip = 53, header = TRUE)
  
  # extract year and place in its own column
  monthly_cpi$year <- year(monthly_cpi$DATE)
  
  # calculate mean CPI for the year
  yearly_cpi <- monthly_cpi %>% 
    group_by(year) %>% 
    summarize(cpi = mean(VALUE))
  
  # calculate inflation rate compared to adjustment year
  yearly_cpi$adj_factor <- yearly_cpi$cpi[yearly_cpi$year == year_adjust]/yearly_cpi$cpi
  
  # combine inflation adjusted wages to wages dataset
  df <- left_join(df, yearly_cpi, by = 'year') %>%
    # adjust income in the given year for inflation since the base year
    # multiply the wage amount in the current year by the adjustment factor
    mutate(estimate_adj = round( !! wages_col * adj_factor, 0 )) %>%
    # recalculate adjusted se, moe , and cv
    mutate(se_adj = round( !! se_col / adj_factor), 0 ) %>%
    mutate(moe_adj = round( se_adj*1.96, 0),
           cv_adj = round( (se_adj / estimate_adj)*100, 2 )) %>%
    # remove unneeded columns
    select(-cpi, -adj_factor)
  
  return(df)
}


ff_remove_comparisons <- function(df) {
  
  ###################################################################
  # This function removes rows containing demographic data
  # for comparison communities in all years except the latest year
  ##################################################################
  
  df %>%   
    filter(!(type != 'Comparison Community' & 
             year != max(.$year) & 
             geo_description != 'Forsyth County, NC'))
  
}


ff_write_to_excel <- function(excel_data, excel_file) {
  
  # inputs:
  #   excel_data: dataset to write to excel
  #   excel_file: file name for excel file that will be written out
    
    # install package to write to excel only if it is needed
    library(openxlsx)
    
    # create workbook to save sheets to
    wb <- createWorkbook()
    
    # create worksheet to store full dataset
    addWorksheet(wb, 'All data') 
    # write all data to first workbook
    writeData(wb, 'All data', excel_data, rowNames = FALSE) 
    
    ### create seperate workbooks for each type of data
    
    # create vector of each type (gender, race, etc);
    # each type will become a different dataset and sheet within the workbook
    types <- distinct(excel_data, type)[[1]]
    
    # iterate through each type
    for (single_type in types) {
      
      # filter data for specific type
      sheet <- excel_data %>%
        filter(type == single_type)
      
      # create workbook
      addWorksheet(wb, single_type) 
      # write data to workbook
      writeData(wb, single_type, sheet, rowNames = FALSE) 
      
    } 
    
    # write out workbook to disk
    # it will be saved in the same folder as the markdown file
    saveWorkbook(wb, excel_file, overwrite=T)
}