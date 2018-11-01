################################################################
# This file creates the data set for employment rates.
################################################################

library(tidyverse)
source('functions/acs/acs_misc_functions.R')

data_files <- list.files('i_civic_engagement/electoral participation/data/PUMS_eligible_voters/')

#2008
eligible_voters_08 <- PUMS_2008_citizens %>%
        filter(!is.na(X1),!str_detect(X1, 'Total'))%>%
        slice(1:18)%>%
        select(1,2)%>%
        rename(subtype = X1, estimate = 'Total GEOG-102')%>%
        mutate(year = str_sub(data_files[1],6,9))

citizen <- eligible_voters_08 %>%
        filter(subtype ==  'Citizen')

eligible_voters_08 <- eligible_voters_08 %>%
        mutate(total_citizens = citizen$estimate)
 
#2010
eligible_voters_10 <- PUMS_2010_citizens %>%
        filter(!is.na(X1),!str_detect(X1, 'Total'))%>%
        slice(1:18)%>%
        select(1,2)%>%
        rename(subtype = X1, estimate = 'Total GEOG-102')%>%
        mutate(year = str_sub(data_files[3],6,9))

citizen <- eligible_voters_10 %>%
        filter(subtype ==  'Citizen')

eligible_voters_10 <- eligible_voters_10 %>%
        mutate(total_citizens = citizen$estimate)

#2012
eligible_voters_12 <- PUMS_2012_citizens %>%
        filter(!is.na(X1),!str_detect(X1, 'Total'))%>%
        slice(1:18)%>%
        select(1,2)%>%
        rename(subtype = X1, estimate = 'Total GEOG-101')%>%
        mutate(year = str_sub(data_files[5],6,9))

citizen <- eligible_voters_12 %>%
        filter(subtype ==  'Citizens')

eligible_voters_12 <- eligible_voters_12 %>%
        mutate(total_citizens = citizen$estimate)

#2014
citizen <- PUMS_2014_citizens %>%
        filter(X1 ==  'Total CIT')

eligible_voters_14 <- PUMS_2014_citizens %>%
        filter(!is.na(X1),!str_detect(X1, 'Total'))%>%
        slice(6:21)%>%
        select(1,2)%>%
        rename(subtype = X1, estimate = 'Total GEOG-101')%>%
        mutate(year = str_sub(data_files[7],6,9))

eligible_voters_14 <- eligible_voters_14 %>%
        mutate(total_citizens = citizen$`Total GEOG-101`)

#2016
citizen <- PUMS_2016_citizens %>%
        filter(X1 ==  'Total CIT')

eligible_voters_16 <- PUMS_2016_citizens %>%
        filter(!is.na(X1),!str_detect(X1, 'Total'))%>%
        slice(5:21)%>%
        select(1,2)%>%
        rename(subtype = X1, estimate = 'Total GEOG-101')%>%
        mutate(year = str_sub(data_files[8],6,9))

eligible_voters_16 <- eligible_voters_16 %>%
        mutate(total_citizens = citizen$`Total GEOG-101`)


elig_voters <- bind_rows(eligible_voters_08,eligible_voters_10,eligible_voters_12,eligible_voters_14,eligible_voters_16)
elig_voters$year <- as.numeric(elig_voters$year)

elig_voters <- elig_voters %>%
        mutate(subtype = replace(subtype, str_detect(subtype, "White"), "White, non-Hispanic")) %>%
        mutate(subtype = replace(subtype, str_detect(subtype, "African"), "African American")) %>%
        mutate(subtype = replace(subtype, str_detect(subtype, "Ind"), "American Indian")) %>%
        mutate(subtype = replace(subtype, str_detect(subtype, "Hispanic"), "Hispanic/Latino")) %>%
        mutate(subtype = replace(subtype, str_detect(subtype, "Asian"), "Asian")) %>%
        mutate(subtype = replace(subtype, str_detect(subtype, "85"), "85 to 99"))%>%
        mutate(subtype = replace(subtype, str_detect(subtype, "18"), "18 to 25"))%>%
        mutate(subtype = replace(subtype, str_detect(subtype, "26"), "26 to 29"))%>%
        mutate(subtype = replace(subtype, str_detect(subtype, "30"), "30 to 39"))%>%
        mutate(subtype = replace(subtype, str_detect(subtype, "40"), "40 to 49"))%>%
        mutate(subtype = replace(subtype, str_detect(subtype, "50"), "50 to 64"))%>%
        mutate(subtype = replace(subtype, str_detect(subtype, "65"), "65 to 84"))

all <- merge(eligible_voters_all_years, tableau_data,by = c("subtype","year"))

votes_age <- merge(age, votes_age, by = c("subtype","year"))

write_csv(votes_age, "votes_by_age.csv")
