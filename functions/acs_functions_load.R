########################################################################################
#
# This file loads all ACS function. When ran in a script, all ACS functions are loaded.
#
########################################################################################

# place everything in a blank function so no variables are loaded into the global environment

acs_load_funcs <- function() {
  
  # locate all files within the ACS functions folder
  files <- list.files('functions/acs')
  
  # run all the files containing ACS functions
  for (f in files) {
    
    # create path to file
    file_path <- paste0('functions/acs/', f)
    
    # run file
    source(file_path)
    
  }
}

# run function to load all ACS functions
acs_load_funcs()