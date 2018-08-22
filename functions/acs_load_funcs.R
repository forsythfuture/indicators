#########################################################################
#
# This file creates and runs a function to load all ACS functions at once.
# Running this file in an R script will load all ACS functions.
#
#########################################################################

# use a function so the global envirnment is not littered with objects
acs_load_func <- function() {
  
  # locate all files within the ACS functions folder
  files <- list.files('../functions/acs')
  
  # run all the files containing ACS functions
  for (f in files) {
    
    # create path to file
    file_path <- paste0('../functions/acs/', f)
    
    # run file
    source(file_path)
    
  }
}

# run function
acs_load_func()