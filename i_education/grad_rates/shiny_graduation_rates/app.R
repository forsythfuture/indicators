library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(kableExtra)

##################### Section to edit ##############################

file_name <- 'grad_rates_cleaned.csv'

indicator_name <- 'Graduation Rates'

# Enter data sources. Each line below represents a single line in the app
data_source <- c('North Carolina Department of Public Instruction<br>', 
                 'http://www.ncpublicschools.org/accountability/reporting/cohortgradrate')

# Enter interpretation of data. Each line below represents a single line in the app
# HTML tags can be used to format text:
# Examples:
#   italics: <i>text</i>
#   bold: <b>text</b>
#   line break: <br>

interpretation <- c('<i>Forsyth County rate</i><br>',
                    'Forsyth County experienced a slight decrease in its graduation rate this past year.',
                    'This decline comes after a general uptick in graduation rates since 2013.',
                    "The drop also caused Forsyth County's graduation rate to fall 2 percentage points",
                    'below the state rate.<br><br>',
                    '<i>Race and Ethnicity</i><br>',
                    'White, non-Hispanics have higher graduation rates than African Americans,',
                    'who in return have higher rates than Hispanic/Latinos<br>',
                    '<i>Economic Status</i><br>',
                    'Economically disadvantaged students experienced a 4% drop in graduation rates',
                    'this past school year.')
################### End section to edit ############################

# load all custom functions
source('global.R')

################# try ################

# # load all datasets into a list
# filenames <- list.files(pattern='.csv')
# # create list of filenames without .csv for naming items in list
# list_names <- str_replace_all(filenames, '.csv', '')

#########################################

# load file
df <- read_csv(file_name) %>%
  select(-level)
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
  select(year, geo_area, geo_description, type, subtype, estimate) %>%
  rename(Year = year, Scope = geo_area, `Geographic Area` = geo_description, 
         Type = type, Subtype = subtype, Estimate = estimate) %>%
  filter(!(Year != '2017-2018' & `Geographic Area` != 'Forsyth County, NC' & Type != 'Total'))

# unique demographics, for drop down menu
unique_demo <- unique(df$type)

# unique years, for z-score checkbox
unique_year <- unique(df$year)

# unique geography, for z-score checkbox
unique_geo <- unique(df$geo_description)


ui <- dashboardPage(
  
  dashboardHeader(title = indicator_name),
  dashboardSidebar(
    
    # table sources
    htmlOutput("source"),
    
    # selectInput("indicator", label = "Indicator:", 
    #             choices = list_names),
    
    # demographic drop down menu
    #uiOutput('demographic')#,
    selectInput("demographic", label = "Demographic:",
                choices = unique_demo,
                selected = unique_demo[1]),
    
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
                 # interpretations
                 htmlOutput("interpretations")
                 ),
        tabPanel("Chi-Square Test",
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
    #source_title <- '<br><b>Data Sources</b><br>'
    #sources <- paste(data_source, collapse='<br>')
    #HTML(paste0(source_title, sources))
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
    data_source_title <- '<br><br><b>Data Source</b><br><br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'
    data_source_list <- paste(data_source, collapse='<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;')
    HTML(paste0(interp_title, interps, data_source_title, data_source_list))
  })
  
  output$ui_demo_check <- renderUI({
    
    checkboxGroupInput('demo_check', 'Demographic:', unique(df_demo()$subtype), selected = unique(df_demo()$subtype), inline = TRUE)
    
  })
  
  output$table_zscore <-  function() {
    
    df_demo() %>%
      filter(year %in% input$year_check,
             geo_description %in% input$geo_check,
             subtype %in% input$demo_check) %>%
      ff_acs_zscore_kable('grads', 'total', c('year', 'geo_description', 'subtype' ))
    
  }
  
  output$table_raw_data <- DT::renderDataTable({
    
    df_demo() %>%
      ff_data_dt(col_names = c('Geography', 'Year', 'Estimate', '95% MOE', 
                               'St. Error', 'CV', 'Type', 'Subtype'))
  })
  
}
shinyApp(ui, server)