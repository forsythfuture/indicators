############################################################################
# This file contains one dataframe that lists the comparison counties. One
# column is the county, the other column is the state.
#
# Comparison counties are located in a stand-alone file so that the comparison
# counties for all r scripts and indicators can be updated by only changing this
# file and rerunning the scripts.
#
# To update comparison counties this script can be ran in the script or
# rmarkdown file where the analysis is being conducted. After running this
# script, the object 'compare' will be created in the global enviromnent
# and it will contain the updated comparison counties.
#
############################################################################

# These are only test comparison counties
compare <- data.frame(county = c('Forsyth', 'Guilford', 'Durham', 'Mecklenburg', 'Wake', 'Cumberland',
                                 'Buncombe', 'Union', 'New Hanover', 'Gaston'),
                      state = rep('NC', 10))