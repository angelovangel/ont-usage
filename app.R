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
library(crosstalk)
library(timevis)
library(stringr)
library(dplyr)
library(lubridate)

# data loaded once for all sessions
files <- list.files('data', pattern = '*.csv', full.names = T, recursive = F)
df <- vroom(files) %>% 
  mutate(type = 'range')

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
      box(width = 12,
          timevisOutput('usage_timevis')
          ),
      box(width = 6, 
          valueBoxOutput('experiments'), 
          valueBoxOutput('cells'), 
          valueBoxOutput('usage')),
      box(width = 6, 
          dataTableOutput('usage_table'))
    )
  )
)

server <- function(input, output, session) {
  
  output$usage_timevis <- renderTimevis({
    timevis(df, groups = groups_df, fit = F, 
            options = list(start = Sys.Date() - 90, end = Sys.Date() + 7)
            )
  })
    
  observe({
    print(input$usage_timevis_selected)
  })
    
}

shinyApp(ui = ui, server = server)
