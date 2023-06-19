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
library(reactable)
library(timevis)
library(stringr)
library(dplyr)
library(lubridate)
library(ivs) # https://cran.r-project.org/web/packages/ivs/vignettes/ivs.html
library(vctrs)
library(digest)
library(scales)

siformat <- function(x) {system2('bin/siformat.sh', args = x, stdout = T)}


# data loaded once for all sessions
df <- vroom('data/df.csv') %>% 
  mutate(group = factor(group))

groups_df <- data.frame(
  id = c('grid', 'prom'),
  content = c('GridION', 'PromethION')
)

# 
ui <- dashboardPage(
  header = dashboardHeader(title = 'ONT machine usage (BCL)', titleWidth = 350),
  sidebar = dashboardSidebar(disable = T), 
  body = dashboardBody(
    fluidRow(
      box(width = 3, 
          dateRangeInput('dates',
                         'Select interval for usage calculation', 
                         min = Sys.Date() - years(5), 
                         max = Sys.Date() + years(1),
                         separator = '--', 
                         start = Sys.Date() - months(3)),
          checkboxInput('stack', 'Expand items', value = FALSE),
          radioButtons('color', 'Color flowcells by', inline = T, 
                       choiceNames = c('Bases', 'Reads', 'Failed %'), 
                       choiceValues = c('gb', 'reads', 'failed'), selected = 'failed')
          ),
      box(width = 9, 
          valueBoxOutput('output'), 
          valueBoxOutput('cells'), 
          valueBoxOutput('usage_prom'),
          valueBoxOutput('usage_grid')
          #uiOutput('selected', inline = T)
          ),
      box(width = 12,
          timevisOutput('usage_timevis')
      )
    )
  )
)

server <- function(input, output, session) {
  # reactives
  dfr <- reactive({
    df[df$start >= input$dates[1] & df$end <= input$dates[2], ]
  })
  
  dfr_merged <- reactive({
    merge_overlaps(df, input$dates[1], input$dates[2], group)
  })
  
  # outputs
  output$usage_timevis <- renderTimevis({
    timevis(df, groups = groups_df, fit = F,
            options = list(
              #start = Sys.Date() - 30, 
              #end = Sys.Date() + 3, 
              min = Sys.Date() - years(5),
              max = Sys.Date() + months(1), 
              maxHeight = '500px', 
              #minHeight = '300px', 
              zoomMin = 604800000, zoomMax = 31556926000, 
              clickToUse = FALSE, 
              stack = input$stack
              )
            ) %>% 
      setSelection(itemId = last(df$id)) %>%
      setWindow(start = input$dates[1], end = input$dates[2])
  })
  
  #dates and timevis are linked
  # observe({
  #   updateDateRangeInput(session, 'dates', start = input$usage_timevis_window[1], end = input$usage_timevis_window[2])
  # })
    
  output$output <- renderValueBox({
    promoutput <- sum(dfr()[dfr()$group == 'prom', ]$bases_pass, na.rm = T)
    gridoutput <- sum(dfr()[dfr()$group == 'grid', ]$bases_pass, na.rm = T)
    totaloutput <- sum(dfr()$bases_pass, na.rm = T)
    #print(selected)
    myvalue <- paste0(siformat(totaloutput), ' bases')
    mysubtitle <- HTML(paste0('prom <b>', siformat(promoutput) , '</b> ',
                              'grid <b>', siformat(gridoutput), ' </b> <br><br>',
                               input$dates[1], ' ', input$dates[2]
    ))
    #print(mysubtitle)
    valueBox(
      color = 'light-blue',
      value = tags$p(myvalue, style = "font-size: 80%; font-weight:normal;"),
      subtitle = mysubtitle
    )
  })
  
  output$cells <- renderValueBox({
    cellcounts <- dfr() %>% count(group, .drop = F)
    myvalue <- paste0(nrow(dfr()), ' flowcells')
    valueBox(
      value = tags$p(myvalue, style = "font-size: 80%; font-weight:normal;"),
      subtitle = HTML(paste0(input$dates[1], ' ', input$dates[2], ' | ',
                        'prom <b>', cellcounts$n[cellcounts$group == 'prom'], '</b> | ',
                        'grid <b>', cellcounts$n[cellcounts$group == 'grid']
                        )),
      color = 'light-blue'
    )
  })
  
  # output$usage_grid <- renderValueBox({
  #   myvalue <- dfr_merged() %>% 
  # })
  
    
}

shinyApp(ui = ui, server = server)
