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

dfmerged <- readRDS('data/dfmerged.rds') %>% 
  mutate(group = factor(group))

dfmerged_total <- dfmerged %>%
  reframe(total_rng = iv_groups(rng)) %>% 
  mutate(start = vctrs::field(total_rng, 1),
         end = vctrs::field(total_rng, 2),
         dur = end - start)

groups_df <- data.frame(
  id = c('grid', 'prom'),
  content = c('GridION', 'PromethION')
)

# 
ui <- dashboardPage(
  skin = 'green',
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
          valueBoxOutput('usage'),
          textOutput('selected_dates')
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
    dfmerged[dfmerged$start <= input$dates[2] & dfmerged$end >= input$dates[1], ]
  })
  
  dfr_merged_total <- reactive({
    dfmerged_total[dfmerged_total$start <= input$dates[2] & dfmerged_total$end >= input$dates[1], ]
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
      #addItems(data = dfmerged) %>%
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
    mysubtitle <- HTML(paste0('prom <b>', siformat(promoutput) , '</b> <br>',
                              'grid <b>', siformat(gridoutput), ' </b> '
    ))
    #print(mysubtitle)
    valueBox(
      color = 'green',
      value = tags$p(myvalue, style = "font-size: 80%; font-weight:normal;"),
      subtitle = mysubtitle
    )
  })
  
  output$cells <- renderValueBox({
    cellcounts <- dfr() %>% count(group, .drop = F)
    myvalue <- paste0(nrow(dfr()), ' flowcells')
    valueBox(
      color = 'green',
      value = tags$p(myvalue, style = "font-size: 80%; font-weight:normal;"),
      subtitle = HTML(paste0(
        'prom <b>', cellcounts$n[cellcounts$group == 'prom'], '</b> <br>',
        'grid <b>', cellcounts$n[cellcounts$group == 'grid']
                        ))
    )
  })
  
  output$usage <- renderValueBox({
    grid_time <- sum(dfr_merged()[dfr_merged()$group == 'grid', ]$dur, na.rm = T) %>% as.duration()
    prom_time <- sum(dfr_merged()[dfr_merged()$group == 'prom', ]$dur, na.rm = T) %>% as.duration()
    total_time <- sum(dfr_merged_total()$dur, na.rm = T) %>% as.duration()
    selected_time <- (input$dates[2] - input$dates[1]) %>% as.duration()
    
    
    total_usage <- paste0(round(total_time/selected_time*100, 0), ' % overall usage')
    grid_usage <- paste0(round(grid_time/selected_time*100, 0), ' %')
    prom_usage <- paste0(round(prom_time/selected_time*100, 0), ' %')
      
    valueBox(value = tags$p(total_usage, style = "font-size: 80%; font-weight:normal;"),
             subtitle = HTML(paste0(
               'prom <b>' , prom_usage, 
               '</b><br> grid <b>', grid_usage
               )),
             color = 'green')
    
  })
  
  output$selected_dates <- renderText({
    paste0(input$dates[1], ' - ', input$dates[2])
  })
  
    
}

shinyApp(ui = ui, server = server)
