library(optparse)
library(vroom)
library(purrr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(timevis)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ivs) # https://cran.r-project.org/web/packages/ivs/vignettes/ivs.html
library(vctrs)
library(highcharter)
#library(digest)
library(scales)
library(httr2)
library(shinymanager)


siformat <- function(x) {system2('bin/siformat.sh', args = x, stdout = T)}

# pb_columns <- c(
#   'run_uniqueId', 'run_instrumentType', 'run_instrumentName', 'run_status', 
#   'cell_name', 'cell_well', 'cell_uniqueId', 'cell_status', 'cell_startedAt', 'cell_completedAt', 'cell_runId', 
#   'run_context','run_startedAt', 'run_completedAt', 'run_name', 'run_status', 'run_numCellsCompleted', 'run_numCellsFailed',
#   'Application', 'Unique Molecular Yield', 'Polymerase Read N50', 'Polymerase Read Length (mean)', 
#   'Polymerase Reads', 'Subread N50','Subread Length (mean)'
# )

# heatmap for some values
# pass x as argument, return hex
my_temp_color <- function(x, xfrom, xto) {
  rainbow <- c("#2ECC71", 
               "#DAF7A6", 
               "#FFC300",
               "#FF5733")
  # set scale 
  # color ramp used for temp gradient
  myramp <- scales::colour_ramp(rev(rainbow), na.color = "#FF5733")
  scaled_x <- scales::rescale( x, from = c(xfrom, xto), to = c(0,1) )
  myramp(scaled_x)
}

# a function, takes a number and returns hex
# used to assign colors to levels of factor
my_levels_color <- brewer_pal(type = 'div', palette = 'Spectral')

# merge overlapping intervals, by group
merge_overlaps <- function(dataframe, start_time, end_time, grouping) {
  dataframe %>%
    ungroup %>%
    mutate(rng = iv({{start_time}}, {{end_time}})) %>%
    reframe(rng = iv_groups(rng), .by = {{grouping}}) %>%
    #rowwise() %>%
    mutate(
      content = paste0({{grouping}}, '-merged'),
      start = vctrs::field(rng, 1),
      end = vctrs::field(rng, 2),
      dur = end - start
      ) 
}

