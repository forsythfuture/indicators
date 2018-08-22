## Explanation of ACS functions

### Import ACS data

```r
ff_import_acs(table_number, state, county, year_start, year_end)
```

This function returns a table for multiple geographic units (county and state combinations) and years.

Parameters:
- table_number: The ACS table number for the data that is needed. This imports all tables in a series. For example, 'B20017' imports 'B20017A', 'B20017B', 'etc.
- state: A vector of strings for the states, which should match with counties.
- county: A vector of strings for the counties, which should match with the states.
- year_start: an integer specifying the first year of data that is needed. Must be greater than 2012.
- year_end: an integer specifying the final year of data that is needed.

*Example*
```r
df <- ff_import_acs(table_number = 'B20017', 
                    state = c('NC', 'NC', 'AR'),
                    county = c('Forsyth', 'Guilford', 'Pulaski'),
                    year_start = 2012,
                    year_end = 2016)
```

*Notes*

The function returns 95% confidence intervals for the margin of error. This is different that the default value of data retrived from American Fact Finder.

### Conduct calculations and significance tests on data

```r
ff_acs_zscore(data_frame, estimate, se, var_names = NULL)
```

This function returns a square symmetrical matrix of z scores for all combinations of values within the data set. The matrix length and with equal the number of rows in the data frame.

The formula comes from: U.S. Census Bureau, A Compass for Understanding and Using ACS Survey Data, A-18 (October 2008)

Parameters:
- data_frame: the dataframe where the estimates and standar errors are housed
- estimate: a string that is the column name of the column containing the estimate
- se: a string that is the column name of the column containing the standard error
- var_names: (optional) a character vector of variables that can be combined to created distinct names for each row and column

*Example*
```r
zscore <- ff_acs_zscore(data_frame = df,
                        estimate = 'estimate', 
                        se = 'se', 
                        var_names = c('GEOID', 'varable', 'year')
```

*Notes*

var_names is optional. It aids in interpreting the matrix by assigning names to the rows and columns. This way, users can more easily trace a specific z-score to which two values created the score.
