library(tidyverse)


# create list of raw data files, including path to directory
directory <- 'i_education/academic_success/data/raw_data/'
# create list of raw data files, including path to directory
raw_data_files <- paste0(directory, list.files(directory))

## import each yearly file, and year to dataset, bind with previous years

# initialize dataframe to store results
df <- data.frame()

for (file in raw_data_files) {
  
  # extract the year number from file name
  acad_year <- str_extract(file, '[0-9][0-9][0-9][0-9]-[0-9][0-9]')

  
 # import dataset
  df <- read_delim(file, "\t", escape_double = FALSE, trim_ws = TRUE) %>%
    # add year
    mutate(year = acad_year) %>%
    bind_rows(df, .)
  
}
df_bu<-df
df <- df_bu

# keep only Forsyth, Durham, Guilford County school data and change date to 4 digit format for charts 
all_data <- df %>%
        filter(school_code=='340LEA'|school_code=='320LEA'|school_code=='410LEA')%>%
        filter(type=='ALL')%>%
        filter(subgroup=='FEM'|subgroup=='EDS'|subgroup=='BLCK'|subgroup=='HISP'|subgroup=='MALE'|subgroup=='NOT_EDS'|subgroup=='WHTE'|subgroup=='ALL')%>%
        mutate(name = ifelse(school_code=='320LEA', 'Durham',
                     ifelse(school_code=='410LEA','Guilford','Forsyth')))%>%
        mutate(subgroup = ifelse(subgroup=='FEM','female', ifelse(subgroup=='MALE', 'male',
                                ifelse(subgroup=="BLCK", 'African American', ifelse(subgroup=="HISP", 'Hispanic/Latino', ifelse(subgroup=="WHTE", 'White, non-Hispanic',
                                        ifelse(subgroup=='EDS','eds',ifelse(subgroup=="NOT_EDS", 'not_eds', 'ALL'))))))))%>%
        mutate(year = ifelse(year=='2013-14','2014', 
                             ifelse(year=='2014-15','2015',
                                    ifelse(year=='2015-16','2016',
                                           ifelse(year=='2016-17','2017','2018')))))
                                                          
dfrd <- filter(all_data, subject=='RD', grade=='03')

dfma <- filter(all_data, subject=='MA', grade=='08')      

dfrdma <- bind_rows(dfrd,dfma)

dfrdma <- dfrdma %>%
        mutate(subject = ifelse(subject=='RD', '3rd Grade Reading Score', '8th Grade Math Score'))%>%
        select('subject', 'year', 'name', 'subgroup','num_tested','pct_glp')
        

colnames(dfrdma) <- c('measure', 'year', 'county', 'group', 'total', 'rate')
       
#Convert variables to numeric type
dfrdma$rate <- as.numeric(dfrdma$rate)
dfrdma$year <- as.integer(dfrdma$year)

#Calculate counts for passed and not passed using rate and total
dfrdma <- dfrdma %>%
        mutate(passed = (rate*total)/100, not_passed = ((100-rate) * total)/100)%>%
        mutate(passed = round(passed,0), not_passed = round(not_passed,0))

# write out dataset
write_csv(dfrdma, 'i_education/academic_success/data/math_reading_scores2.csv')














```{r}
# create list of raw data files, including path to directory
directory <- 'i_education/academic_success/data/raw_data/'
# create list of raw data files, including path to directory
raw_data_files <- paste0(directory, list.files(directory))

## import each yearly file, and year to dataset, bind with previous years

# initialize dataframe to store results
df <- data.frame()

for (file in raw_data_files) {
        
        # extract the year number from file name
        acad_year <- str_extract(file, '[0-9][0-9][0-9][0-9]-[0-9][0-9]')
        
        
        # import dataset
        df <- read_delim(file, "\t", escape_double = FALSE, trim_ws = TRUE) %>%
                # add year
                mutate(year = acad_year) %>%
                bind_rows(df, .)
        
}

# filter out unneeded rows and rename values
df <- df %>%
        filter(school_code=='340LEA'|school_code=='320LEA'|school_code=='410LEA')%>%
        filter(type=='ALL')%>%
        filter(subgroup=='FEM'|subgroup=='EDS'|subgroup=='BLCK'|subgroup=='HISP'|subgroup=='MALE'|subgroup=='NOT_EDS'|subgroup=='WHTE'|subgroup=='ALL')%>%
        mutate(name = ifelse(school_code=='320LEA', 'Durham',
                             ifelse(school_code=='410LEA','Guilford','Forsyth')))%>%
        mutate(subgroup = ifelse(subgroup=='FEM','female', ifelse(subgroup=='MALE', 'male',
                                                                  ifelse(subgroup=="BLCK", 'black', ifelse(subgroup=="HISP", 'hispanic', ifelse(subgroup=="WHTE", 'white',
                                                                                                                                                ifelse(subgroup=='EDS','eds',ifelse(subgroup=="NOT_EDS", 'not_eds', 'ALL'))))))))


dfrd <- filter(df, subject=='RD', grade=='03')

dfma <- filter(df, subject=='MA', grade=='08')      

df <- bind_rows(dfrd,dfma)

df <- df %>%
        mutate(subject = ifelse(subject=='RD', '3rd Grade Reading Score', '8th Grade Math Score'))%>%
        select('subject', 'year', 'name', 'subgroup','num_tested','pct_glp')


colnames(df) <- c('measure', 'year', 'county', 'group', 'total', 'rate')

#Convert variables to numeric type
df$rate <- as.numeric(df$rate)

#Calculate counts for passed and not passed using rate and total
df <- df %>%
        mutate(passed = (rate*total)/100, not_passed = ((100-rate) * total)/100)%>%
        mutate(passed = round(passed,0), not_passed = round(not_passed,0))

# write out dataset
write_csv(df, 'i_education/academic_success/data/math_reading_scores.csv')

# to make plots need to have year in 4 digit format
df4yearformat <- df %>%
        mutate(year = ifelse(year=='2013-14','2014', 
                             ifelse(year=='2014-15','2015',
                                    ifelse(year=='2015-16','2016',
                                           ifelse(year=='2016-17','2017','2018')))))

rdma2018 <- rdma

```

