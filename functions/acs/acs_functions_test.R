#############################################################################
#
# Data calculations
#
# The following functions perform calculations on ACS data,
# such as adding proportions and conducting significance tests.
# change
##############################################################################


ff_acs_ratios <- function(num_estimate, num_moe, den_estimate, den_moe) {
  
  # This function calculates the ratios, se, and 95% margins of error for the ratios
  # The inputs are as follows:
  #   num_estimate: The dataframe and column of the numerator in ratio
  #   num_moe: The dataframe and column of the numerator's moe
  #   den_estimate: The dataframe and column of the denominator in ratio
  #   den_moe: The dataframe and column of the denominator's moe
  #
  # The formula comes from:
  #    U.S. Census Bureau, A Compass for Understanding and Using ACS Survey Data, A-15
  #
  # The output is the same dataframe with the ratio and margin of error added to the dataframe
  
  # calculate the ratio 
  ratio  <- round( num_estimate / den_estimate, 4 )
  
  # calculate MOE
  # uses the tidycensus package
  moe <- round( moe_ratio(num_estimate, den_estimate, num_moe, den_moe), 4 )
  
  # calcuate standard error
  # note: since 95% confidence intervals are used, the moe is divided
  # by 1.96, not 1.645 when using the 90% moe's from AFF
  se <- round( moe / 1.96, 4)
  
  # calculate cv
  cv <- round( (se / ratio) * 100, 2 )
  
  # add ratio, MOE, se and cv to dataframe
  df <- data.frame(ratio = ratio,
                   moe = moe,
                   se = se,
                   cv = cv)

  return(df)
  
}


ff_acs_perc <- function(num_estimate, num_moe, den_estimate, den_moe) {
  
  # This function calculates the percentages / proportiion, se, and 95% margins of error for the percentages
  # The inputs are as follows:
  #   num_estimate: The dataframe and column of the numerator in percentages
  #   num_moe: The dataframe and column of the numerator's moe
  #   den_estimate: The dataframe and column of the denominator in percentages
  #   den_moe: The dataframe and column of the denominator's moe
  #
  # The formula comes from:
  #    U.S. Census Bureau, A Compass for Understanding and Using ACS Survey Data, A-15
  #
  # The output is the same dataframe with the ratio and margin of error added to the dataframe
  
  # calculate the proportion 
  prop  <- round( num_estimate / den_estimate, 4 )
  
  # calcualte standard errors of num and den, used to calcualte prop se
  num_se <- num_moe / 1.96
  den_se <- den_moe / 1.96
  
  # calculate proportion se
  x <- num_se^2 - (prop^2 * den_se^2)
  
  # formula to use depends on whether x is positive
  if (x < 0) {
    
    se <- sqrt(x) / den_estimate
    
  } else {
    
    x <- num_se^2 + (prop^2 * den_se^2)
    
    se <- sqrt(x) / den_estimate
    
  }
  
  # calculate MOE
  # uses the tidycensus package
  #moe <- round( moe_prop(num_estimate, den_estimate, num_moe, den_moe), 4 )
  
  # calcuate MOES
  # note: since 95% confidence intervals are used, the se is multiplied by 1.96, not 1.645
  moe <- se * 1.96
  
  # calculate cv
  cv <- (se / prop) * 100
  
  # add ratio, MOE, se and cv to dataframe
  df <- data.frame(prop = prop,
                   moe = round( moe, 4 ),
                   se = round( se, 4 ),
                   cv = round( cv, 2))
  
  return(df)
  
}


ff_acs_zscore <- function(data_frame, estimate, se, var_names = NULL) {
  
  # This function returns a square symetrical matrix of z scores for all combinations of values
  # The matrix length and with equal the number of rows in the data frame
  #
  # The formula comes from:
  #    U.S. Census Bureau, A Compass for Understanding and Using ACS Survey Data, A-18
  #
  # Parameters:
  #   data_frame: the dataframe where the estimates and se are housed
  #   estimate: a string that is the column name of the column containing the estimate
  #   se: a string that is the column name of the column containing the se
  #   var_names: a character vector of variables that can be combined to created
  #              distinct names for each row and column
  
  
  # initialize an empty data frame with one column and the same number
  # of rows as the final dataframe
  z_score_mat <- data.frame(n = seq(1, nrow(data_frame)))
  
  # iterate through each row in the dataframe
  for (i in 1:nrow(data_frame)) {
    
    # calculate the point estimate differences and the sum of
    # of standard errors for the given row and all other rows
    # this will return a vector
    estimate_diff <- data_frame[[i, estimate]] - data_frame[[estimate]]
    se_diff <- sqrt( data_frame[[i, se]]^2 + data_frame[[se]]^2 )
    
    # calculate the z score for all row values, rounds to two decimals
    z_score <- abs( estimate_diff / se_diff) %>% round(2)

    # add the row of z scores to the z score matrix
    z_score_mat[, i] <- z_score
    
  }
  
  if (!is.null(var_names)) {
    
    # if there is only one variable name, then use this as the label
    # otherwise paste together variable names
    if (length(var_names) == 1) {
      
      # sometime isolating a column returns a data frame, and sometimes it returns a vector
      # if a dataframe is returned, isolate first, and only, column as a vector
      if (is.data.frame(unique(data_frame[ , var_names])) == TRUE) {
        
        names_vec <- unique(data_frame[ , var_names])[[1]]
      
      } else {

        names_vec <- unique(data_frame[ , var_names])
        
      }
      
    } else {
      
      # create vector of label names by pasting columns together
      names_vec <- apply( data_frame[ , var_names], 1, paste, collapse = ": " )
      
    }
    
    # shorted names so they appear cleaner and shorter in the matrix as column and row headers
    
    # replace any United States and North Carolina values with NC and US
    names_vec <- str_replace_all(names_vec, 'United States', 'US') %>%
      str_replace_all('North Carolina', 'NC') %>%
      str_replace_all(' County, NC', '') %>%
      # replace and ethnicities with abbreviation
      str_replace_all('African American', 'AA') %>%
      str_replace_all('Hispanic/Latino', 'HL') %>%
      str_replace_all('White, non-Hispanic', 'Wh') %>%
      # shorten age descriptions (take off the word 'year')
      str_replace_all(' years', '') %>%
      str_replace_all(' and over', '+') %>%
      # shorten age by converting 'to' to '-'
      str_replace_all(' to ', '-') %>%
      # remove word 'ratio;
      str_replace_all(' ratio', '')
      
    # add labels as column and row names
    colnames(z_score_mat) <- names_vec
    row.names(z_score_mat) <- names_vec
    
  }
  
  return(z_score_mat)
  
}


ff_acs_zscore_kable <- function(data_frame, estimate, se, var_names = NULL, table_name = 'Z-Scores') {
  
  # This function takes as input a matrix of z score generated by ff_acs_zscore
  # it returns a kable table of z scores with scores over 1.96 in bold
  
  # input:
  #   zscore_matrix: matrix of z-scores generated from ff_acs_zscore
  #   table_name: table caption name for kable table
  
  data_frame %>%
    ff_acs_zscore(estimate, se, var_names) %>%
    # bold any z score over 1.96
    mutate_all(funs(cell_spec(., bold = ifelse(. > 1.96, T,F)))) %>%
    # add column names as the first row because row names do not print
    mutate(Compare = colnames(.),
           # bold column of column / row names
           Compare = cell_spec(Compare, bold = T)) %>%
    # make the comparison column (column and row names) the first column
    select(Compare, everything()) %>%
    # create kable table
    kable(caption = table_name, escape = F)  %>%
    # add formating (every other row in gray)
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = F) %>%
    # bold row names
    column_spec(1, bold = T)
}


ff_acs_zscore_dt <- function(data_frame, estimate, se, var_names = NULL) {
  
  # This function takes as input a dataframe and returns a datatable table of z scores.
  # The input is the same input that is requires for `ff_acs_zscore`
  #
  # Input:
  #   data_frame: the dataframe where the estimates and se are housed
  #   estimate: a string that is the column name of the column containing the estimate
  #   se: a string that is the column name of the column containing the se
  #   var_names: a character vector of variables that can be combined to created
  #              distinct names for each row and column
  
  z_mat <- ff_acs_zscore(data_frame, estimate, se, var_names)
  
  datatable(z_mat, filter='top', extensions = c('FixedColumns', 'Buttons'), 
            options = list(scrollX = TRUE, fixedColumns = TRUE, dom = 'Bfrtip')) %>%
  formatStyle(columns = names(z_mat),
              backgroundColor = styleInterval(1.96, c('white', 'gray')),
              color = styleInterval(1.96, c('black', 'white')))
  
}


ff_acs_zplot <- function(zscore_matrix) {
  
  # This function returns a plot similair to a correlatin plot,
  # showing whether z values are significant
  # Its input is a z sore matrix generated from ff_acs_zscore

  # identify column names of first and last columns, so they can be used to gather data
  first <- names(zscore_matrix[1])
  last <- names(zscore_matrix[ncol(zscore_matrix)])
  
  # add column of row names to matrix
  zscore_matrix$row_names <- row.names(zscore_matrix)
  
  # gather values to create long form data set
  zscore_matrix %>% gather(first:last, key = 'var1', value = 'value') %>%
  # plot
  ggplot(aes(row_names, var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 1.96, space = "Lab",
                         name="Z-Score Matrix\nBlue < 1.96\nRed > 1.96") +
    labs(title = 'Z score significance',
         x = '', y = '') +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed()

}