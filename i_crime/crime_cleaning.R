####Import Data ####

data <- read.csv("ViolentCrimeRate.csv")

####Reformat Geography to match template ####

data$geo_description[data$ï..Geography == "Forsyth"] <-"Forsyth County, NC"
data$geo_description[data$ï..Geography == "Durham"] <- "Durham County, NC"
data$geo_description[data$ï..Geography == "Guilford"] <- "Guilford County, NC"
data$geo_description[data$ï..Geography == "North Carolina"] <- "North Carolina" 

#### making year lower case #### 

data$year <- data$Year

#### adding subtype ####

data$subtype <- "Total"

####  adding type #### 

data$type <- "Comparison Community"

#### Subsetting dataframes by crime type 

violentdata <- select(data, year, geo_description, Violent.Rate, type, Population, subtype)
murderdata <- select(data, year, geo_description, Murder.Rate, type, Population, subtype)
rapedata <- select(data, year, geo_description, Rape.Rate, type, Population, subtype)
robberydata <- select(data, year, geo_description, Robbery.Rate, type, Population, subtype)
assaultdata <- select(data, year, geo_description, Assault.Rate, type, Population, subtype)


#### Renaming rates to estimate 

violentdata$estimate <- violentdata$Violent.Rate
murderdata$estimate <- murderdata$Murder.Rate
rapedata$estimate <- rapedata$Rape.Rate
robberydata$estimate <- robberydata$Robbery.Rate
assaultdata$estimate <- assaultdata$Assault.Rate

#### Calculate SE 

violentdata$se <- sqrt((violentdata$estimate/violentdata$Population)*100000)
murderdata$se <- sqrt((murderdata$estimate/murderdata$Population)*100000)
rapedata$se <- sqrt((rapedata$estimate/rapedata$Population)*100000)
robberydata$se <- sqrt((robberydata$estimate/robberydata$Population)*100000)
assaultdata$se <- sqrt((assaultdata$estimate/assaultdata$Population)*100000)

####      final grouping of needed columns 

violentdata <- select(violentdata, year, geo_description, subtype,
                      estimate, se, type)
murderdata <- select(murderdata, year, geo_description, subtype,
                     estimate, se, type)
rapedata <- select(rapedata, year, geo_description, subtype,
                   estimate, se, type)
robberydata <- select(robberydata, year, geo_description, subtype,
                      estimate, se, type)
assaultdata <- select(assaultdata, year, geo_description, subtype,
                      estimate, se, type)

####  export csv 

write.csv(violentdata, "violentdata.csv")
write.csv(murderdata, "murderdata.csv")
write.csv(rapedata, "rapedata.csv")
write.csv(robberydata, "robberydata.csv")
write.csv(assaultdata, "assaultdata.csv")

####  clean data for use with shiny app

library(tidyverse)

# create list of raw data files
data_files <- list.files('i_crime/data', full.names = TRUE)

# create vector of descriptive crime names, to add as column
crime_names <- c('Assault', 'Murder', 'Rape', 'Robbery', 'Violent')

# initialize dataset that will contain all crimes
crimes <- data.frame()

# iterate through each file and crime, import file, add crime name as column,
# and merge with dataset containg all crimes
for (i in seq_along(data_files)){
  
  # import file containing single crime
  crimes <- read_csv(data_files[i]) %>%
    # add subtype column that is the crime name
    mutate(subtype = crime_names[i]) %>%
    # bind to dataframe containg all crimes
    bind_rows(crimes, .)
    
}

# change violent crime to 'total' for subtype, to match format
crimes <- crimes %>%
  mutate(subtype = str_replace_all(subtype, 'Violent', 'Total'),
         # if the crime is not total voilent crime, make type 'Other violent crime'
         type = ifelse(subtype != 'Total', 'Other violent crime', type))

# write out dataset to folder with shiny app
write_csv(crimes, 'i_crime/shiny_crime/crime.csv')
  
