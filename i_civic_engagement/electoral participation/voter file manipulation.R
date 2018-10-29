write_csv(voter_history, 'i_civic_engagement/electoral participation/data/voter_history.csv')
write_csv(elections, 'i_civic_engagement/electoral participation/data/elections.csv')


df <- read_csv('i_civic_engagement/electoral participation/data/voter_history.csv')

elections <- read_csv('i_civic_engagement/electoral participation/data/election_numbers.csv')


voter_history <- Voter.History.05.08.2018 %>%
        select(1,5:9,11:14,23:26)

df <- df %>%
  filter(!str_detect(ELECTION.NAME, 'MUNICIPAL'),!str_detect(ELECTION.NAME, 'SECOND'), !is.na(ELECTION.NAME))

elections <- df %>%
        group_by(ELECTION.NAME)%>%
        summarise(count=n())%>%
        mutate(year = str_sub(ELECTION.NAME,7,10))%>%
        arrange(year)








public.voter.102218 <- public.voter.102218 %>%
        mutate(year = str_sub(REGISTRATION.DATE,7,10))%>%
        arrange(year)




elections <- elections %>%
  filter(!str_detect(ELECTION.NAME, 'MUNICIPAL'),!str_detect(ELECTION.NAME, 'SECOND'), !is.na(ELECTION.NAME))

df_NC <- NC_fc_voter_file %>%
        filter(!is.na(county_id))

df_fc_votes16 <- df_votes16

df_NC$election_lbl <- as.character(df_NC$election_lbl)

df_NC_votes16 <- df_NC %>%
        filter(election_lbl=='2016-11-08') %>%
        rename(reg_num = voter_reg_num)%>%
        select(3,4,6,7)

match_NC_FC <- merge(df_fc_votes16,df_NC_votes16, by = 'reg_num')


df_fc_votes16 <- df_fc_votes16 %>%
        mutate(matchNC = ifelse(df_fc_votes16$reg_num == df_NC_votes16$reg_num, "yes","no"))

NC_voter_stats_20161108$county_desc <- as.character(NC_voter_stats_20161108$county_desc)

wsjournal_NC_file <- NC_voter_stats_20161108 %>%
        filter(county_desc=="FORSYTH")

sum(wsjournal_NC_file$total_voters)

results_pct_20161108$County <- as.character(results_pct_20161108$County)

wsresults_NC_file <- results_pct_20161108 %>%
        filter(County=="FORSYTH")

sum(wsresults_NC_file$Total.Votes)
