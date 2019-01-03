######################################
# functions for shiny app
######################################

# function to create line charts
plotly_plots <- function(df, input_type) {
  
  df %>%
    #filter(type == !!input_type) %>%
    ggplot(aes(quintile, estimate, fill=subtype)) +
    geom_bar(stat='identity', position='dodge', color='black', size=.2) +
    geom_errorbar(aes(ymin = estimate-se*1.96, ymax = estimate+se*1.96), 
                  position = position_dodge(width=.9), width=.25) +
    geom_hline(yintercept = .2, size = .2) +
    #facet_wrap(~geo_description) +
    labs(x='\nIncome Percentile',
         y='Percentage of demographic in each income percentile',
         fill='Race/Ethnicity') +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_minimal() +
    theme(axis.text.x=element_text(angle=45, hjust=1))
  
}


z_score_table <- function(list_data, df, demo) {
  # function that creates z scores
  
  
  # create dataset to be used for z scores
  zscore_df <- list_data[[df]] #%>%
    # only keep selected demographic
    filter(type == demo)
  
  ff_acs_zscore(zscore_df, 'estimate', 'se', 
                c('geo_description', 'year', 'subtype'))
  
}


ff_acs_zscore <- function(data_frame, estimate, se, test, 
                          success = NULL, trials = NULL, var_names = NULL) {
  
  # This function returns a square symetrical matrix of of all significance tests for all combinations of values
  # The function can calculate either a z-score or a p-value from a Chi-Square test
  # The matrix length and with equal the number of rows in the data frame
  #
  # The z-score formula comes from:
  #    U.S. Census Bureau, A Compass for Understanding and Using ACS Survey Data, A-18
  #
  # Parameters:
  #   data_frame: the dataframe where the estimates and se are housed
  #   estimate: a string that is the column name of the column containing the estimate
  #   se: a string that is the column name of the column containing the se
  #   var_names: a character vector of variables that can be combined to created
  #              distinct names for each row and column
  #   test: either 'z' or 'chi-square'
  #   success: if chi-square, number of successes
  #   trials: if chi-square, number of trials
  
  
  # initialize an empty data frame with one column and the same number
  # of rows as the final dataframe
  z_score_mat <- data.frame(n = seq(1, nrow(data_frame)))
  
  if (test == 'z') {
    
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
    
  } else if (test == 'chi-square') {
    
    
    # create vectors of counts and totals, 
    # leads to shorter code than refering to column names
    success_c <- data_frame[[success]]
    trials_c <- data_frame[[trials]]
    
    # iterate through each row in the dataframe
    for (i in 1:nrow(data_frame)) {
      
      # conduct proportion test for between value at row in loop and all other valyes
      p_value <- sapply(1:nrow(data_frame), 
                        function(x) prop.test(c(success_c[i],success_c[x]),c(trials_c[i],trials_c[x]))$p.value)
      
      # add the row of z scores to the z score matrix
      z_score_mat[, i] <- round(p_value, 3)
      
    } 
    
  } else {
    
    stop("Test must be either 'z' or 'chi-square'")
    
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

ff_acs_zscore_kable <- function(data_frame, estimate, se, test,
                                success = NULL, trials = NULL, var_names = NULL, 
                                table_name = 'Significance test') {
  
  # This function takes as input a matrix of z score generated by ff_acs_zscore
  # it returns a kable table of z scores with scores over 1.96 in bold
  
  # input:
  #   zscore_matrix: matrix of z-scores generated from ff_acs_zscore
  #   table_name: table caption name for kable table
  
  # for z-score we want to bold anything over 1.96,
  # for chi-square, we want to bold anything under 0.05
  # each of these values represent the significance threshold
  thresh <- if (test == 'z') 1.96 else 0.05
  # we want to bold numbers over threshold for z and under threshold for chi-square
  # due to this difference, we must create TRUE and FALSE values of whether to bold
  # depending on what test is used
  if_true_bold <- if (test =='z') T else F
  if_false_bold <- if (test =='z') F else T
  
  data_frame %>%
    ff_acs_zscore(estimate, se, test = test, success = success, trials = trials, var_names = var_names) %>%
    # bold any z score over 1.96
    mutate_all(funs(cell_spec(., 
                              bold = ifelse(. > thresh, 
                                            if_true_bold,
                                            if_false_bold)))) %>%
    # add column names as the first row because row names do not print
    mutate(Compare = colnames(.),
           # bold column of column / row names
           Compare = cell_spec(Compare, bold = T)) %>%
    # only keep rows of Forsyth County
    filter(str_detect(Compare, 'Forsyth')) %>%
    # make the comparison column (column and row names) the first column
    select(Compare, everything()) %>%
    # create kable table
    kable(caption = table_name, escape = F)  %>%
    # add formating (every other row in gray)
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left", font_size = 10) %>%
    # bold row names
    column_spec(1, bold = T)
}

ff_data_dt <- function(df, col_names, for_tableau=FALSE, trials = FALSE) {
  
  # Input:
  #   df: a dataframe of raw data that you want convert to a DT datatable
  #   col_names: column names for the datatable
  #   for_tableau: whether this table is for tableau output
  #   trials: whether the data contains columns of successes and trials
  #
  # To color cv values, the cv column must be names 'cv'
  #
  # Note: Do not use this to create a table of z-scores; use ff_acs_zscore_dt
  
  if (for_tableau == FALSE) {
    
    datatable(df,
              filter='top', extensions='Buttons', rownames = FALSE,
              colnames = col_names,
              options = list(scrollX = TRUE, scrollY = TRUE, dom = 'Bfrtip')) %>%
      # color cv numbers based on value, only if column named 'cv' exists
      formatStyle('cv', color = styleInterval(c(12, 30), c('black', 'blue', 'red')))
    
  } else {
    
    # if the table is for tableau, we need to add additional rows that represent Forsyth County totals,
    # but change type from Comparison Community to Total
    
    df %>%
      # filter for rows with Forsyth County as the county, and where type starts with Comparison
      filter(str_detect(geo_description, '^Forsyth'),
             str_detect(type, '^Comparison')) %>%
      # change type columns from Comparison to Total
      mutate(type = 'Total') %>%
      # bind these rows to the original dataframe
      bind_rows(df)  %>%
      # create datatable
      datatable(filter='top', extensions='Buttons', rownames = FALSE,
                colnames = col_names,
                options = list(scrollX = TRUE, scrollY = TRUE, dom = 'Bfrtip'))
    
  }
}