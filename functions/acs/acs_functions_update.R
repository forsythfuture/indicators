############################################################################################################
#
# This function automatically updates the ACS data in the csv files. It can be used when new ACS data drops.
#
#############################################################################################################


ff_update_acs <- function(master_df_path, table_num, state, county, current_year) {
  
  # This function updates the csv files by adding the current year's data
  
  # import current year ACS data
  update_df <- ff_import_acs(table_number, state, county, current_year, current_year)
  
  # import local copy of master csv dataset and bind updated data to it
  master_df <- read_csv(master_df_path) %>%
    bind_rows(update_df)
  
  return(master_df)
}