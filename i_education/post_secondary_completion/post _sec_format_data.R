library(dplyr)
library(stringi)
library(stringr)

edu_made <- read.csv('edu_made.csv')

edu_made2 <- edu_made %>%
  filter(str_detect (NAME, "Pct_Assoc"))

Forsyth <- edu_made2 %>%
  select(1,6)

Forsyth <- Forsyth %>% 
  rename_at( 2, ~"Forsyth" )

#Forsyth_Est$Name <- as.character(Forsyth_Est$Name)

Forsyth_Est <- Forsyth %>%
  filter(str_detect (NAME, "\\d$"))%>%
  rename(Forsyth_Est=Forsyth)

Forsyth_SEs <- Forsyth %>%
  filter(str_detect (NAME, "_SE"))%>%
  rename(Forsyth_SE=Forsyth)

Forsyth_SEs$NAME <- substr(Forsyth_SEs$NAME,1,19)

Forsyth_Est <- merge(Forsyth_Est,Forsyth_SEs,by = "NAME")

Forsyth_Est <- Forsyth_Est %>%
        mutate(Forsyth_CV = 100*(Forsyth_SE/Forsyth_Est))

#Pulaski
Pulaski <- edu_made2 %>%
        select(1,2)

Pulaski <- Pulaski %>% 
        rename_at( 2, ~"Pulaski" )

Pulaski_Est <- Pulaski %>%
        filter(str_detect (NAME, "\\d$"))%>%
        rename(Pulaski_Est=Pulaski)

Pulaski_SEs <- Pulaski %>%
        filter(str_detect (NAME, "_SE"))%>%
        rename(Pulaski_SE=Pulaski)

Pulaski_SEs$NAME <- substr(Pulaski_SEs$NAME,1,19)

Pulaski_Est <- merge(Pulaski_Est,Pulaski_SEs,by = "NAME")

Pulaski_Est <- Pulaski_Est %>%
        mutate(Pulaski_CV = 100*(Pulaski_SE/Pulaski_Est))

#Lafayette
Lafayette <- edu_made2 %>%
        select(1,3)

Lafayette <- Lafayette %>% 
        rename_at( 2, ~"Lafayette" )

Lafayette_Est <- Lafayette %>%
        filter(str_detect (NAME, "\\d$"))%>%
        rename(Lafayette_Est=Lafayette)

Lafayette_SEs <- Lafayette %>%
        filter(str_detect (NAME, "_SE"))%>%
        rename(Lafayette_SE=Lafayette)

Lafayette_SEs$NAME <- substr(Lafayette_SEs$NAME,1,19)

Lafayette_Est <- merge(Lafayette_Est,Lafayette_SEs,by = "NAME")

Lafayette_Est <- Lafayette_Est %>%
        mutate(Lafayette_CV = 100*(Lafayette_SE/Lafayette_Est))

#Jackson
Jackson <- edu_made2 %>%
        select(1,4)

Jackson <- Jackson %>% 
        rename_at( 2, ~"Jackson" )

Jackson_Est <- Jackson %>%
        filter(str_detect (NAME, "\\d$"))%>%
        rename(Jackson_Est=Jackson)

Jackson_SEs <- Jackson %>%
        filter(str_detect (NAME, "_SE"))%>%
        rename(Jackson_SE=Jackson)

Jackson_SEs$NAME <- substr(Jackson_SEs$NAME,1,19)

Jackson_Est <- merge(Jackson_Est,Jackson_SEs,by = "NAME")

Jackson_Est <- Jackson_Est %>%
        mutate(Jackson_CV = 100*(Jackson_SE/Jackson_Est))

#NC
NC <- edu_made2 %>%
        select(1,5)

NC <- NC %>% 
        rename_at( 2, ~"NC" )

NC_Est <- NC %>%
        filter(str_detect (NAME, "\\d$"))%>%
        rename(NC_Est=NC)

NC_SEs <- NC %>%
        filter(str_detect (NAME, "_SE"))%>%
        rename(NC_SE=NC)

NC_SEs$NAME <- substr(NC_SEs$NAME,1,19)

NC_Est <- merge(NC_Est,NC_SEs,by = "NAME")

NC_Est <- NC_Est %>%
        mutate(NC_CV = 100*(NC_SE/NC_Est))

#Guilford
Guilford <- edu_made2 %>%
        select(1,7)

Guilford <- Guilford %>% 
        rename_at( 2, ~"Guilford" )

Guilford_Est <- Guilford %>%
        filter(str_detect (NAME, "\\d$"))%>%
        rename(Guilford_Est=Guilford)

Guilford_SEs <- Guilford %>%
        filter(str_detect (NAME, "_SE"))%>%
        rename(Guilford_SE=Guilford)

Guilford_SEs$NAME <- substr(Guilford_SEs$NAME,1,19)

Guilford_Est <- merge(Guilford_Est,Guilford_SEs,by = "NAME")

Guilford_Est <- Guilford_Est %>%
        mutate(Guilford_CV = 100*(Guilford_SE/Guilford_Est))

#Roanoke
Roanoke <- edu_made2 %>%
        select(1,8)

Roanoke <- Roanoke %>% 
        rename_at( 2, ~"Roanoke" )

Roanoke_Est <- Roanoke %>%
        filter(str_detect (NAME, "\\d$"))%>%
        rename(Roanoke_Est=Roanoke)

Roanoke_SEs <- Roanoke %>%
        filter(str_detect (NAME, "_SE"))%>%
        rename(Roanoke_SE=Roanoke)

Roanoke_SEs$NAME <- substr(Roanoke_SEs$NAME,1,19)

Roanoke_Est <- merge(Roanoke_Est,Roanoke_SEs,by = "NAME")

Roanoke_Est <- Roanoke_Est %>%
        mutate(Roanoke_CV = 100*(Roanoke_SE/Roanoke_Est))

Ed_Attain <- merge(Forsyth_Est,NC_Est,by = "NAME")
Ed_Attain <- merge(Ed_Attain, Guilford_Est,by = "NAME")
Ed_Attain <- merge(Ed_Attain, Pulaski_Est,by = "NAME")
Ed_Attain <- merge(Ed_Attain, Lafayette_Est,by = "NAME")
Ed_Attain <- merge(Ed_Attain, Jackson_Est,by = "NAME")
Ed_Attain <- merge(Ed_Attain, Roanoke_Est,by = "NAME")

Ed_Attain <- Ed_Attain %>%
        mutate(Age = substr(NAME,12,15), 
               Gender = substr(NAME,16,17),
               Race_Eth = substr(NAME,16,17),
               Year = substr(NAME,18,19))

Ed_Attain_backup <- Ed_Attain
Ed_Attain <- Ed_Attain_backup


Ed_Attain$Gender <- ifelse(Ed_Attain$Gender == "FA", "Female", ifelse(Ed_Attain$Gender == "MA", "Male", "ALL"))

Ed_Attain$Race_Eth <- ifelse(Ed_Attain$Race_Eth == "AW", "White", 
                             ifelse(Ed_Attain$Race_Eth == "AB", "Black",
                                    ifelse(Ed_Attain$Race_Eth == "AH", "Hispanic", "ALL")))

Ed_Attain$Year <- paste0("20",Ed_Attain$Year)  

Ed_Attain <- Ed_Attain %>%
        select(1,26,23,25,24, ends_with("_Est"), ends_with("_SE"), ends_with("_CV"))

write.csv(Ed_Attain, "Ed_Attain.csv")































































































































































