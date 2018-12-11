library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(kableExtra)

##################### Section to edit ##############################
#shiny_indicators/
file_name <- 'crime.csv'

indicator_name <- 'Violent Crime'

# Enter data sources. Each line below represents a single line in the app
data_source <- c('')

# enter the type of significance test
# either 'z' for z-test or 'chi-square' for chi-square test
sig_test <- 'z'

# Enter interpretation of data. Each line below represents a single line in the app
# HTML tags can be used to format text:
# Examples:
#   italics: <i>text</i>
#   bold: <b>text</b>
#   line break: <br>

interpretation <- c('')

################### End section to edit ############################

# load all custom functions
source('global.R')

#########################################

# matrix function;
# if the test is a z test, these two variables are NULL
success <- if (sig_test == 'chi-square') 'success' else NULL
trials <- if (sig_test == 'chi-square') 'trials' else NULL

# create name for significance testing matrix, signifying which values are statistically significant
chi_table_name <- 'Chi-square test p-values: Values under 0.05 (in bold) are statistically significant'
z_table_name <- 'Z scores: Values above 1.96 (in bold) are statistically significant'

# load file
df <- read_csv(file_name) %>%
  # create MOE and CV
  mutate(moe = 1.96 * se,
         cv = (se/estimate) * 100)

# ensure columns are in proper order
# number of columns will depend on whether trials and successes are included
# data_cols signify the column names that will show in the DT table in the shiny app
if (sig_test == 'chi-square') {
  
  df <- df %>%
    select(geo_description, year, type, subtype, 
           estimate, success, trials, moe, se, cv)
  
  data_cols <- c('Geography', 'Year', 'Type', 'Subtype',
                 'Estimate', 'Successes', 'Trials', 'St. Error', '95% MOE', 'CV')
  
} else {
  df <- df %>%
    select(geo_description, year, type, subtype, 
           estimate, moe, se, cv)
  
  data_cols <- c('Geography', 'Year', 'Type', 'Subtype',
                 'Estimate', 'St. Error', '95% MOE', 'CV')
  
}

#
# crate tableau dataset
tableau_df <- df %>%
  # filter for rows with Forsyth County as the county, and where type starts with Comparison
  filter(str_detect(geo_description, '^Forsyth'),
         str_detect(type, '^Comparison')) %>%
  # change type columns from Comparison to Total
  mutate(type = 'Total') %>%
  # bind these rows to the original dataframe
  bind_rows(df) %>%
  select(year, geo_description, type, subtype, estimate) %>%
  rename(Year = year, `Geographic Area` = geo_description, Type = type,
         Subtype = subtype, Estimate = estimate)

# unique demographics, for drop down menu
unique_demo <- unique(df$type)

# unique years, for z-score checkbox
unique_year <- unique(df$year)

# unique geography, for z-score checkbox
unique_geo <- unique(df$geo_description)


ui <- dashboardPage(
  
  dashboardHeader(title = indicator_name),
  dashboardSidebar(
    
    # fileInput("dataset", "Import CSV file:",
    #           accept = c(
    #             "text/csv",
    #             "text/comma-separated-values,text/plain",
    #             ".csv")
    #),
    tags$hr(),
    # radioButtons("sig_test", "Significance Test:",
    #              c("Z-score" = "zscore",
    #                "Chi-Square" = "chisquare",
    #                "Log-normal" = "lnorm")),
    
    # demographic drop down menu
    #uiOutput('demographic')#,
    selectInput("demographic", label = "Demographic:",
                choices = unique_demo,
                selected = 'Comparison Community'),
    
    # download tableau data buttom
    downloadButton("download_tableau", "Download Tableau")
    
  ),
  
  dashboardBody(
    fluidRow(
      tabsetPanel(
        tabPanel("Plots", 
                 plotlyOutput("plot_line"),
                 tags$hr(),
                 plotlyOutput("plot_bar"),
                 # table sources
                 htmlOutput("source"),
                 # interpretations
                 htmlOutput("interpretations")
                 ),
        tabPanel(if (sig_test == 'z') "Z Scores" else if (sig_test == 'chi-square') "Chi-Square Test" else "Error",
                 checkboxGroupInput('year_check', 'Years:', unique_year, selected = max(unique_year), inline = TRUE),
                 checkboxGroupInput('geo_check', 'Geography:', unique_geo, selected = 'Forsyth County, NC', inline = TRUE),
                 uiOutput("ui_demo_check"),
                 tableOutput('table_zscore')
                 ),
        tabPanel("Raw Data",
                 dataTableOutput('table_raw_data'))
      )
    )
  )
)

server <- function(input, output, session) { 
  
  # create dataset based on which demographic is selected
  df_demo <- eventReactive(input$demographic, {
    df %>%
      filter(type == input$demographic)
  })
  
  # output data sources
  output$source <- renderUI({
    source_title <- '<br><b>Data Sources</b><br>'
    sources <- paste(data_source, collapse='<br>')
    HTML(paste0(source_title, sources))
  })
  
  output$demographic <- renderUI(
    
    selectInput("demographic", label = "Demographic:",
                choices = unique_demo(),
                selected = NULL)
    
  ) 

  output$download_tableau <- downloadHandler(
    filename = function() {
      paste0('tableau_', file_name)
    },
    content = function(con) {
      write.csv(tableau_df, con, row.names = FALSE)
    },
    contentType = 'text/csv'
  )
  
  output$plot_line <- renderPlotly({
    
    plotly_plots(df_demo(), input$demographic, 'line')
    
  })
  
  output$plot_bar <- renderPlotly({
    
    plotly_plots(df_demo(), input$demographic, 'bar')
    
  })
  
  # output interpretations
  output$interpretations <- renderUI({
    
    # '&nbsp;' dds additional spaces to indent line
    interp_title <- '<br><b>Interpretation</b><br><br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'
    interps <- paste(interpretation, collapse='<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;')
    HTML(paste0(interp_title, interps))
  })
  
  output$ui_demo_check <- renderUI({
    
    checkboxGroupInput('demo_check', 'Demographic:', unique(df_demo()$subtype), selected = unique(df_demo()$subtype), inline = TRUE)
    
  })
  
  output$table_zscore <-  function() {
    
    df_demo() %>%
      filter(year %in% input$year_check,
             geo_description %in% input$geo_check,
             subtype %in% input$demo_check) %>%
      ff_acs_zscore_kable('estimate', 'se', test = sig_test, success = success, trials = trials,
                          var_names = c('year', 'geo_description', 'subtype' ),
                          table_name = if (sig_test == 'z') z_table_name else chi_table_name)
    
  }
  
  output$table_raw_data <- DT::renderDataTable({
    
    df_demo() %>%
      ff_data_dt(col_names = data_cols)
  })
  
}
shinyApp(ui, server)