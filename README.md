# Forsyth Futures community indicators

This repo includes all indicators. Folders separate indicators by sector. Within each sector folder lies a data folder, where all the data for that sector is housed. R scripts to generate the data lie in each sector's folder.

The *acs_functions* file in the main directory stores common functions used to import and manipulate ACS data with the tidycensus and acs packages.

Accessing ACS data through an API requires installing a census key. To install the key, first go to https://api.census.gov/data/key_signup.html and sign up for a key. Then, install and load the *tidycensus* package within R. Finally, install the census key by running the following R command: 

```r
'census_api_key("[entre censuss key]", install = TRUE).
```