##################################################################
#
# This function imports income data and expands by weight
# so that entire distribution of incomes can be plotted
#
##################################################################

library(tidyverse)
library(DBI)
library(data.table)
library(gridExtra)
library(grid)

source('i_social_justice/palma_functions.R')
source('functions/puma_functions.R')
source('functions/acs/acs_misc_functions.R')

# connect to PUMS database
con <- dbConnect(RSQLite::SQLite(), "puma_data/pums_db.db")

# function to create grid with one legend
grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}


# create function to import incomes based on year parameter
income_year <- function(con, yr) {

  state <- 37
  area_code = c(1801, 1802, 1803)
  year <- yr
  
  # vector that will return all states when filtered
  all_states <- seq(1, 100)
  # vector to return all PUMAs
  all_pumas <- seq(1, 10000)
  
  # create table names
  yr <-str_extract(as.character(year), '[0-9][0-9]$')
  income_table <- paste0('h_', yr)
  
  # establish connection to tables
  income <- tbl(con, income_table)
  
  # import these PUMS variables
  income_vars <- c('TYPE', 'PUMA', 'ST', 'HINCP', 'WGTP')
  
  # import incomes
  income <- income %>%
    select(!!income_vars) %>%
    filter(# state and PUMA filter
      # if state or PUMA is na, use vector containing all states and pumas for filtering
      ST == 37,
      PUMA %in% c(1801, 1802, 1803),
      TYPE == 1, # housing units only
      (!is.na(HINCP) & HINCP >= 0)) %>% # positive household income
    select(-TYPE) %>%
    select(HINCP, WGTP, PUMA) %>%
    mutate(year = year) %>%
    collect() %>%
    as.data.table()
  
  # use weights to repeat rows
  income <- income[rep(seq(.N), WGTP), !"WGTP"]
  
  return(income) 
}

# initialize dataframe to store all year results
income <- data.frame()

# loop through 2014 - 2017, extracting incomes
for (i in 2017) {
  
  df <- income_year(con, i)
  
  income <- bind_rows(income, df)
  
}

ranks <- c(.10, .20, .40, .80, .90)

# find quintiles
quintiles_puma <- income %>%
  group_by(PUMA) %>%
  summarise(rank = list(quantile(HINCP, prob= ranks))) %>% 
  unnest() %>%
  mutate(percentile = rep(!!ranks, 3))
  
  
  summarize(quantile(.$HINCP, probs = c(.10, .20, .40, .80, .90)))

quantile(income$HINCP, probs = c(.10, .20, .40, .80, .90))

# create transformations
income <- income %>%
  as.tibble() %>%
  mutate(PUMA = as.factor(PUMA),
         PUMA = fct_recode(PUMA, `North Winston-Salem` = "1801", 
                           `South Winston-Salem` = "1802",
                           `Kernersville and Clemmons` = '1803'))

# adjust for inflation
income <- ff_inflation_adjust(income, wages_col = HINCP, year_adjust = 2014, error = FALSE)

# calculate income quintiles for each year
income_quintiles <- income %>%
  group_by(year) %>%
  summarise(quantile = list(quantile(estimate_adj, prob= c(.01, .05,.10, .20,.40,.60,.90, .99)))) %>% unnest()

# create kde

# function to calculate breaks
#plot_breaks <- function(x) {as.vector(quantile(x, prob= c(.05,.20,.40,.60,.90)))}
plot_breaks <- function(x) {income_quintiles[income_quintiles[['year']] == x, 'quantile'][[1]]}


# initialize list containing plots
plot_list <- list()
i = 1
# iterate through each year creating plots
for (yr in seq(2014, 2017)) {
  plot_list[[i]] <- income %>%
    filter(year == yr) %>%
    ggplot(aes(estimate_adj, colour = PUMA, fill = PUMA)) +
      geom_density(alpha = 0.1, adjust = 2) +
      facet_wrap(~year, scales = 'free_x') +
      scale_x_continuous('Income',
                         breaks = plot_breaks(yr),
                         trans = 'log',
                         label=scales::dollar_format()) +
      ylim(0, .6) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # only place title on first plot
  if (i == 1) {
    plot_list[[i]] <- plot_list[[i]] +
      labs(title = 'Distribution of incomes by PUMA')
  }
  
  # remove y axis labels on right hand side plots (even numbers)
  if (i %% 2 == 0) {
    plot_list[[i]] <- plot_list[[i]] +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
  }
  
  i = i + 1

}


grid_arrange_shared_legend(plot_list[[1]], plot_list[[2]], 
                           plot_list[[3]], plot_list[[4]])
