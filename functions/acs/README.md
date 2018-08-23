## Explanation of ACS functions

### Import ACS data
_____

```{r}
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
```{r}
acs_df <- ff_import_acs(table_number = 'B20017', 
                        state = c('NC', 'NC', 'AR'),
                        county = c('Forsyth', 'Guilford', 'Pulaski'),
                        year_start = 2012,
                        year_end = 2016)
```

*Notes*

The function returns 95% confidence intervals for the margin of error. This is different than the default value of data retried from American Fact Finder.
_____
### Conduct calculations and significance tests on data
_____

```{r}
ff_acs_zscore(data_frame, estimate, se, var_names = NULL)
```

This function returns a square symmetrical matrix of z scores for all combinations of values within the data set. The matrix length and with equal the number of rows in the data frame.

The formula comes from: U.S. Census Bureau, A Compass for Understanding and Using ACS Survey Data, A-18 (October 2008)

Parameters:
- data_frame: the data frame where the estimates and standard errors are housed
- estimate: a string that is the column name of the column containing the estimate
- se: a string that is the column name of the column containing the standard error
- var_names: (optional) a character vector of variables that can be combined to created distinct names for each row and column

*Example*
```{r}
zscore <- ff_acs_zscore(data_frame = acs_df,
                        estimate = 'estimate', 
                        se = 'se', 
                        var_names = c('GEOID', 'varable', 'year'))
```

*Notes*

var_names is optional. It aids in interpreting the matrix by assigning names to the rows and columns. This way, users can more easily trace a specific z-score to which two values created the score.
_____

```{r}
ff_acs_zplot(zscore_matrix)
```

This function returns a plot similar to a correlation plot, showing whether z values are significant. Z scores over 1.96 are significant, which corresponds to a p-value of 0.05 or less. Red values are statistically significant, blue values are not.

The function's input is a z sore matrix generated from `r ff_acs_zscore`.

*Example*
```{r}
ff_acs_zplot(zscore_matrix = zscore)
```
Here is an example of the output:

![alt text](https://github.com/forsythfuture/indicators/blob/master/functions/acs/zscore_plot.png)

_____
### Filter and clean ACS data
_____

```{r}
ff_acs_ethnicity(df)
```

This function removes ethnicities from an ACS dataset that are not regularly used due to a lack of data. Its input is an ACS data frame created by `r ff_import_acs`.

The following ACS ethnicities are retained:
- ALL
- BLACK OR AFRICAN AMERICAN ALONE
- WHITE ALONE, NOT HISPANIC OR LATINO
- HISPANIC OR LATINO

*Example*
```{r}
df_ethnicities <- ff_acs_ethnicity(df = acs_df)
```

_____

```r
ff_acs_keep_vars(df, variables)
```
  
This program filters for specific variables. The variables are the three digit numbers that are shown as the last three digits in the 'variables' column. Input includes the dataframe of ACS data and the variables that are needed variables are entered as a three digit string (ex: '001').

Important: variables must be entered as strings.

Parameters:
- df: an ACS data frame created by `r ff_import_acs`
- variables: a string vector of variables to retain

*Example*
```{r}
df_vars <- ff_acs_keep_vars(df = df_ethnicities,
                            variables = c('001', '003', '007'))
```
_____


    