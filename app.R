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


# data loaded once for all sessions
processed_files <- list.files('data', pattern = 'grid.csv|prom.csv', full.names = T, recursive = F)
count_files <- list.files('data', pattern = 'counts.csv', full.names = T, recursive = F)
df1 <- vroom(processed_files) %>% 
  dplyr::distinct() %>%
  mutate(type = 'range') %>%
  rowwise() %>% 
  mutate(id = run_id)
  #mutate(id = digest(rnorm(1)))
  # create id
 
siformat <- function(x) {system2('bin/siformat.sh', args = x, stdout = T)}

df2 <- vroom(count_files, col_names = c('file', 'bases_pass', 'bases_fail', 'reads_pass', 'reads_fail')) %>%
  dplyr::distinct() %>%
  # to get colors based on bases, before siformat
  mutate(ratio = bases_fail/bases_pass, style = paste0('background-color: ', my_temp_color(ratio, 0, 1), ';')) %>%
  mutate_at('bases_pass', siformat) %>%
  mutate(flowcell = str_extract(string = file, pattern = '(?<=summary_)[A-Z]+[0-9]+'))

df <- df1 %>% 
  left_join(df2, by = c('seq_summary_file' = 'file')) %>%
  arrange(start)

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
      box(width = 3, 
          dateRangeInput('dates',
                         'Select interval for usage calculation', 
                         min = Sys.Date() - years(5), 
                         max = Sys.Date() + years(1),
                         separator = '--', 
                         start = Sys.Date() - months(3)),
          checkboxInput('stack', 'Expand items', value = FALSE),
          radioButtons('color', 'Color by', inline = T, 
                       choiceNames = c('Gb output', 'Failed %'), 
                       choiceValues = c('output', 'failed'), selected = 'failed')
          ),
      box(width = 9, 
          valueBoxOutput('selected'), 
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
    
  output$selected <- renderValueBox({
    selected <- df$id == input$usage_timevis_selected
    #print(selected)
    myvalue <- ifelse(
      length(selected) == 0,
      '',
      paste0(df$bases_pass[selected], ' bases')
                      )
    
    mysubtitle <- ifelse(
        length(selected) == 0,
        '',
      paste0(df$group[selected], ' | ', df$content[selected], ' | ', 
      round(interval(df$start[selected], df$end[selected]) %>% as.duration() %>% as.numeric('hours'), 1), ' hours', ' | ',
      round(df$ratio[selected]*100, 0), '% failed'
      )
      )
    #print(mysubtitle)
    valueBox(
      color = 'light-blue',
      value = myvalue,
      subtitle = mysubtitle
    )
  })
  
  output$cells <- renderValueBox({
    cellcounts <- dfr() %>% count(group)
    valueBox(
      value = paste0(nrow(dfr()), ' flowcells'), 
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
