library(dplyr)
library(stringi)
library(stringr)

edu_made <- read.csv('edu_made.csv')

#edu_made <- edu_made[-1,]

#edu_made <- as.data.frame(edu_made)

edu_made2 <- edu_made %>%
  filter(str_detect(NAME, "Pct_Assoc"))

Forsyth <- edu_made2 %>%
  select(1,5)

Forsyth <- Forsyth %>% 
  rename_at( 2, ~"Forsyth" )

#Forsyth_Est$Name <- as.character(Forsyth_Est$Name)

Forsyth_Est <- Forsyth %>%
  filter(str_detect (NAME, "\\d$"))%>%
  rename(Forsyth_Est=Forsyth)

Forsyth_SEs <- Forsyth %>%
  filter(str_detect (NAME, "_SE"))%>%
  rename(Forsyth_SE=Forsyth)

#Remove _SE from NAME to enable merge
Forsyth_SEs$NAME <- substr(Forsyth_SEs$NAME,1,19)

Forsyth_Est <- merge(Forsyth_Est,Forsyth_SEs,by = "NAME")

Forsyth_Est <- Forsyth_Est %>%
        mutate(Forsyth_CV = 100*(Forsyth_SE/Forsyth_Est))

#Durham
Durham <- edu_made2 %>%
        select(1,4)

Durham <- Durham %>% 
        rename_at( 2, ~"Durham" )

Durham_Est <- Durham %>%
        filter(str_detect (NAME, "\\d$"))%>%
        rename(Durham_Est=Durham)

Durham_SEs <- Durham %>%
        filter(str_detect (NAME, "_SE"))%>%
        rename(Durham_SE=Durham)

Durham_SEs$NAME <- substr(Durham_SEs$NAME,1,19)

Durham_Est <- merge(Durham_Est,Durham_SEs,by = "NAME")

Durham_Est <- Durham_Est %>%
        mutate(Durham_CV = 100*(Durham_SE/Durham_Est))

#NC
NC <- edu_made2 %>%
        select(1,3)

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
        select(1,6)

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


Ed_Attain <- merge(Forsyth_Est,NC_Est,by = "NAME")
Ed_Attain <- merge(Ed_Attain, Guilford_Est,by = "NAME")
Ed_Attain <- merge(Ed_Attain, Durham_Est,by = "NAME")

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
        select(1,17,14,16,15, ends_with("_Est"), ends_with("_SE"), ends_with("_CV"))

write.csv(Ed_Attain, "Ed_Attain.csv")































































































































































