################################################################
# This file creates the data set for employment rates.
################################################################

library(tidyverse)
source('functions/acs/acs_misc_functions.R')

data_files <- list.files('i_civic_engagement/electoral participation/data/DF files election/')

#2008
eligible_voters_08 <- X2008_DF_citizen_s %>%
        filter(!is.na(X1),!str_detect(X1, 'Total'))%>%
        slice(1:18)%>%
        select(1,2)%>%
        rename(subtype = X1, estimate = 'Total GEOG-102')%>%
        mutate(year = str_sub(data_files[1],1,4))

citizen <- eligible_voters_08 %>%
        filter(subtype ==  'Citizen')

eligible_voters_08 <- eligible_voters_08 %>%
        mutate(total_citizens = citizen$estimate)
 
#2009
eligible_voters_09 <- X2009_DF_citizen_s %>%
        filter(!is.na(X1),!str_detect(X1, 'Total'))%>%
        slice(1:18)%>%
        select(1,2)%>%
        rename(subtype = X1, estimate = 'Total GEOG-102')%>%
        mutate(year = str_sub(data_files[2],1,4))

citizen <- eligible_voters_09 %>%
        filter(subtype ==  'Citizen')

eligible_voters_09 <- eligible_voters_09 %>%
        mutate(total_citizens = citizen$estimate)

#2010
eligible_voters_10 <- X2010_DF_citizen_s %>%
        filter(!is.na(X1),!str_detect(X1, 'Total'))%>%
        slice(1:18)%>%
        select(1,2)%>%
        rename(subtype = X1, estimate = 'Total GEOG-102')%>%
        mutate(year = str_sub(data_files[3],1,4))

citizen <- eligible_voters_10 %>%
        filter(subtype ==  'Citizen')

eligible_voters_10 <- eligible_voters_10 %>%
        mutate(total_citizens = citizen$estimate)

#2011
eligible_voters_11 <- X2011_DF_citizen_s %>%
        filter(!is.na(X1),!str_detect(X1, 'Total'))%>%
        slice(1:18)%>%
        select(1,2)%>%
        rename(subtype = X1, estimate = 'Total GEOG-102')%>%
        mutate(year = str_sub(data_files[4],1,4))

citizen <- eligible_voters_11 %>%
        filter(subtype ==  'Citizen')

eligible_voters_11 <- eligible_voters_11 %>%
        mutate(total_citizens = citizen$estimate)

#2012
eligible_voters_12 <- X2012_DF_citizen_s %>%
        filter(!is.na(X1),!str_detect(X1, 'Total'))%>%
        slice(1:18)%>%
        select(1,2)%>%
        rename(subtype = X1, estimate = 'Total GEOG-101')%>%
        mutate(year = str_sub(data_files[5],1,4))

citizen <- eligible_voters_12 %>%
        filter(subtype ==  'Citizens')

eligible_voters_12 <- eligible_voters_12 %>%
        mutate(total_citizens = citizen$estimate)

#2013
eligible_voters_13 <- X2013_DF_citizen_s %>%
        filter(!is.na(X1),!str_detect(X1, 'Total'))%>%
        slice(1:18)%>%
        select(1,2)%>%
        rename(subtype = X1, estimate = 'Total GEOG-101')%>%
        mutate(year = str_sub(data_files[6],1,4))

citizen <- eligible_voters_13 %>%
        filter(subtype ==  'Citizens')

eligible_voters_13 <- eligible_voters_13 %>%
        mutate(total_citizens = citizen$estimate)

#2014
citizen <- X2014_DF_citizen_s %>%
        filter(X1 ==  'Total CIT')

eligible_voters_14 <- X2014_DF_citizen_s %>%
        filter(!is.na(X1),!str_detect(X1, 'Total'))%>%
        slice(6:21)%>%
        select(1,2)%>%
        rename(subtype = X1, estimate = 'Total GEOG-101')%>%
        mutate(year = str_sub(data_files[7],1,4))

eligible_voters_14 <- eligible_voters_14 %>%
        mutate(total_citizens = citizen$`Total GEOG-101`)

#2016
citizen <- X2016_DF_citizen_s %>%
        filter(X1 ==  'Total CIT')

eligible_voters_16 <- X2016_DF_citizen_s %>%
        filter(!is.na(X1),!str_detect(X1, 'Total'))%>%
        slice(5:21)%>%
        select(1,2)%>%
        rename(subtype = X1, estimate = 'Total GEOG-101')%>%
        mutate(year = str_sub(data_files[8],1,4))

eligible_voters_16 <- eligible_voters_16 %>%
        mutate(total_citizens = citizen$`Total GEOG-101`)


votes_age <- bind_rows(eligible_voters_08,eligible_voters_10,eligible_voters_12,eligible_voters_14,eligible_voters_16)
votes_age$year <- as.numeric(votes_age$year)

all <- merge(eligible_voters_all_years, tableau_data,by = c("subtype","year"))

votes_age <- merge(age, votes_age, by = c("subtype","year"))



write_csv(votes_age, "votes_by_age.csv")

*****
2016
eligible_voters_14 <- X2014_DF_citizen_s %>%
        filter(!is.na(X1),!str_detect(X1, 'Total'))%>%
        slice(1:18)%>%
        select(1,2)%>%
        rename(subtype = X1, estimate = 'Total GEOG-102')%>%
        mutate(year = str_sub(data_files[i],1,4))

citizen <- eligible_voters_14 %>%
        filter(subtype ==  'Citizen')

eligible_voters_14 <- eligible_voters_14 %>%
        mutate(citizens = citizen$estimate)





data_files <- list.files('i_civic_engagement/electoral participation/data/DF files election')     

df_full <- data.frame()
for (i in seq_along(data_files)) {
        
        temp <- read_csv(data_files[i])
        temp <- temp %>%
        
                filter(!is.na(X1),!str_detect(X1, 'Total'))%>%
                slice(1:18)%>%
                select(1,2)%>%
                rename(subtype = X1, estimate = 'Total GEOG-102')%>%
                mutate(year = str_sub(data_files[i],1,4))
        
        # bind to previous year
        df_full <- df_full %>%
                bind_rows(df)
}

return(df_full)

}

x <- str_sub(data_files[1],1,4)

function(zip_file, raw_data_path, years) {
        
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


      





















# folder to put raw data into
data_path <- 'i_civic_engagement/electoral participation/data'

# full path and file name of zip file
zip_file <- 'zip_files/DF files election.zip'

# import and clean employment data
df <- import(zip_file,
                    data_path, 
                    years = seq(2006, 2017, 1))

# write out data frame
write_csv(df, 'i_civic_engagement/electoral participation/data/eligible_voters.csv')

unzip_files <- function(file_path, output_dir) {
        
        # This function takes a zipped file of ACS data, unzips it,
        # and only keeps the data files
        
        # Input:
        # file_path: path to zip file
        # output_dir: directory for outpt
        
        # unzip files
        unzip(file_path, exdir = output_dir)
        
}

import <- function(zip_file, raw_data_path, years) {
        
        # This file takes as input a .zip file of AFF downloaded data
        # and outputs a sinlge cleaned data set of all the files in the .zip file
        
        # input:
        #  zip_file: the file name and full path to the zip file
        #  raw_data_path: The folder that the raw data should be copied to
        #  years: vector of years represented in the data
        
        # unzip files
        # they will be temporarily stored in the same folder as the zip files
        unzip_files(zip_file, raw_data_path)
        
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