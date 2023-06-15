#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(optparse)
library(vroom)
library(purrr)
library(shiny)
library(shinydashboard)
library(DT)
library(timevis)
library(stringr)
library(dplyr)
library(lubridate)
library(digest)

# data loaded once for all sessions
processed_files <- list.files('data', pattern = '*.csv', full.names = T, recursive = F)
df1 <- vroom(processed_files) %>% 
  dplyr::distinct() %>%
  mutate(type = 'range') %>%
  # create id
  rowwise() %>% 
  mutate(id = digest(rnorm(1)))

count_files <- list.files('data/counts/', pattern = '*.csv', full.names = T, recursive = F)
dcounts <- vroom(count_files, col_names = c('file', 'bases', 'reads')) %>%
  mutate(flowcell = str_extract(string = file, pattern = '(?<=summary_)[A-Z]+[0-9]+'))

groups_df <- data.frame(
  id = c('grid', 'prom'),
  content = c('GridION', 'PromethION')
)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  header = dashboardHeader(title = 'ONT machine usage'),
  sidebar = dashboardSidebar(disable = T), 
  body = dashboardBody(
    fluidRow(
      box(width = 3, dateRangeInput('dates', 'Select interval', separator = '--')),
      box(width = 9, 
          valueBoxOutput('experiments'), 
          valueBoxOutput('cells'), 
          valueBoxOutput('usage'),
          valueBoxOutput('selected')
          ),
      box(width = 12,
          timevisOutput('usage_timevis')
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$usage_timevis <- renderTimevis({
    timevis(df1, groups = groups_df, fit = F, 
            options = list(
              start = Sys.Date() - 90, 
              end = Sys.Date() + 7, 
              min = Sys.Date() - years(3), 
              max = Sys.Date() + months(1), 
              maxHeight = '500px', 
              minHeight = '300px', 
              zoomMin = 604800000, zoomMax = 31556926000, clickToUse = TRUE
              )
            )
  })
    
  output$selected <- renderValueBox({
    selected <- df1$id == input$usage_timevis_selected
    valueBox(
      color = 'light-blue',
      value = paste0(
        round(interval(df1$start[selected], df1$end[selected]) %>% as.duration() %>% as.numeric('hours'), 1), 
        ' hours'
        ),
      subtitle = paste0(df1$content[selected], ' | ', df1$group[selected])
    )
  })
  
    
}

shinyApp(ui = ui, server = server)
