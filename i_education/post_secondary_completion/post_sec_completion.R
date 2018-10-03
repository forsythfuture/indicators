################################################################
library(tidyverse)
library(tidycensus)
source('../../i_education/post_secondary_completion/post_sec_functions.R')

edu<-1

#ACS and more intuitive race/ethnic labels
race_eth_label <- c("W","B","H")
race_eth_acs <- c("H","B","I")


for (year in c(2017,2016,2015,2014,2013,2012,2011,2010,2009,2008,2007)){
 #for (year in c(2006)){
  
  
  yr2 <- substr(year,3,4)
  
  
  #get totals and gender from B15002
  edu <- acs_newvar(edu,"B15002_001","Pop",age1="25",year=year)
  edu <- acs_newvar(edu,"B15002_002","Pop",age1="25",MF="M",year=year)
  edu <- acs_newvar(edu,"B15002_019","Pop",age1="25",MF="F",year=year)
  edu <- acs_add_sumofvars(edu,c("B15002_014","B15002_015","B15002_016","B15002_017","B15002_018"),"Assoc_p",age1="25",MF="M",year=year)
  edu <- acs_add_sumofvars(edu,c("B15002_031","B15002_032","B15002_033","B15002_034","B15002_035"),"Assoc_p",age1="25",MF="F",year=year)
  
  edu <- acs_add(edu,paste0("Assoc_p2599FA",yr2),paste0("Assoc_p2599MA",yr2),paste0("Assoc_p2599AA",yr2))
  
  edu <- acs_proportion(edu,paste0("Assoc_p2599AA",yr2),paste0("Pop2599AA",yr2),paste0("Pct_Assoc_p2599AA",yr2))
  edu <- acs_proportion(edu,paste0("Assoc_p2599MA",yr2),paste0("Pop2599MA",yr2),paste0("Pct_Assoc_p2599MA",yr2))
  edu <- acs_proportion(edu,paste0("Assoc_p2599FA",yr2),paste0("Pop2599FA",yr2),paste0("Pct_Assoc_p2599FA",yr2))
  
  
  #race/ethnicity
  
  #in 2008 a separate category for GED was added
  for (i in 1:3){
    edu <- acs_newvar(edu,paste0("B15002",race_eth_acs[i],"_001"),"Pop",age1="25",year=year,race_eth=race_eth_label[i])
    
    if(year>2007){
      
      edu <- acs_add_sumofvars(edu,c(paste0("B15002",race_eth_acs[i],"_008"),paste0("B15002",race_eth_acs[i],"_009"),paste0("B15002",race_eth_acs[i],"_010"),paste0("B15002",race_eth_acs[i],"_017"),paste0("B15002",race_eth_acs[i],"_018"),paste0("B15002",race_eth_acs[i],"_019")),"Assoc_p",age1="25",race_eth=race_eth_label[i],year=year)
    }else{
      edu <- acs_add_sumofvars(edu,c(paste0("B15002",race_eth_acs[i],"_007"),paste0("B15002",race_eth_acs[i],"_008"),paste0("B15002",race_eth_acs[i],"_009"),paste0("B15002",race_eth_acs[i],"_015"),paste0("B15002",race_eth_acs[i],"_016"),paste0("B15002",race_eth_acs[i],"_017")),"Assoc_p",age1="25",race_eth=race_eth_label[i],year=year)
    }
    edu <- acs_proportion(edu,paste0("Assoc_p2599A",race_eth_label[i],yr2),paste0("Pop2599A",race_eth_label[i],yr2),paste0("Pct_Assoc_p2599A",race_eth_label[i],yr2))
  }
  
  
  
  #age
  edu <- acs_add_sumofvars(edu,c("B15001_011","B15001_052"),"Pop",age1="25",age2="34",year=year)
  edu <- acs_add_sumofvars(edu,c("B15001_019","B15001_060"),"Pop",age1="35",age2="44",year=year)
  edu <- acs_add_sumofvars(edu,c("B15001_027","B15001_068"),"Pop",age1="45",age2="64",year=year)
  edu <- acs_add_sumofvars(edu,c("B15001_035","B15001_076"),"Pop",age1="65",age2="99",year=year)
  
  edu <- acs_add_sumofvars(edu,c("B15001_016","B15001_017","B15001_018","B15001_057","B15001_058","B15001_059"),"Assoc_p",age1="25",age2="34",year=year)
  edu <- acs_add_sumofvars(edu,c("B15001_024","B15001_025","B15001_026","B15001_065","B15001_066","B15001_067"),"Assoc_p",age1="35",age2="44",year=year)
  edu <- acs_add_sumofvars(edu,c("B15001_032","B15001_033","B15001_034","B15001_073","B15001_074","B15001_075"),"Assoc_p",age1="45",age2="64",year=year)
  edu <- acs_add_sumofvars(edu,c("B15001_040","B15001_041","B15001_042","B15001_081","B15001_082","B15001_083"),"Assoc_p",age1="65",age2="99",year=year)
  
  edu <- acs_proportion(edu,paste0("Assoc_p2534AA",yr2),paste0("Pop2534AA",yr2),paste0("Pct_Assoc_p2534AA",yr2))
  edu <- acs_proportion(edu,paste0("Assoc_p3544AA",yr2),paste0("Pop3544AA",yr2),paste0("Pct_Assoc_p3544AA",yr2))
  edu <- acs_proportion(edu,paste0("Assoc_p4564AA",yr2),paste0("Pop4564AA",yr2),paste0("Pct_Assoc_p4564AA",yr2))
  edu <- acs_proportion(edu,paste0("Assoc_p6599AA",yr2),paste0("Pop6599AA",yr2),paste0("Pct_Assoc_p6599AA",yr2))
  
}


names <- colnames(edu)

edu_made <- edu

edu_made <- t(edu_made)

write.csv(edu_made, "edu_made.csv")
