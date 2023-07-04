#
# ont-usage Shiny app (BCL)

library(optparse)
library(vroom)
library(purrr)
library(shiny)
library(shinydashboard)
library(DT)
library(timevis)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ivs) # https://cran.r-project.org/web/packages/ivs/vignettes/ivs.html
library(vctrs)
library(highcharter)
library(digest)
library(scales)

siformat <- function(x) {system2('bin/siformat.sh', args = x, stdout = T)}


# data loaded once for all sessions
df <- vroom('data/df.csv') %>% 
  mutate(group = factor(group))
flowcells <- na.exclude(df$flowcell)
  

dfmerged <- df %>% 
  merge_overlaps(start, end, group) %>% 
  mutate(group = factor(group))
# dfmerged <- readRDS('data/dfmerged.rds') %>% 
#   mutate(group = factor(group))

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
  header = dashboardHeader(title = 'TGS machine usage (BCL)', titleWidth = 350),
  sidebar = dashboardSidebar(disable = T), 
  body = dashboardBody(
    tabsetPanel(
      tabPanel(title = "ONT usage",
    
    fluidRow(
      box(width = 3, 
          dateRangeInput('dates',
                         'Select interval for usage calculation', 
                         min = Sys.Date() - years(5), 
                         max = Sys.Date() + years(1),
                         separator = '--', 
                         start = Sys.Date() - months(3)),
          # selectizeInput('sampleids', 'Search by Pool ID', 
          #                choices = NA, 
          #                multiple = T),
          checkboxInput('stack', 'Expand items', value = FALSE),
          radioButtons('color', 'Color flowcells by', inline = T,
                       choiceNames = c('Mean Q-score', 'Failed bases %'),
                       choiceValues = c('style_qscore', 'style_failed'), selected = 'style_failed')
                       # these match the df.csv columns
          ),
  
      box(width = 9, 
          valueBoxOutput('output'), 
          valueBoxOutput('cells'), 
          valueBoxOutput('usage'),
          htmlOutput('selected_dates')
          ),
      box(width = 12,
          timevisOutput('usage_timevis')
          #downloadButton('download', 'Download data'),
      ),
      box(width = 12,
          DTOutput('datatable')
          )
    )
  ),
  tabPanel('ONT output',
           fluidRow(
             box(
               width = 3, 
               dateRangeInput('dates2', 'Select dates for output calculation', 
                              min = Sys.Date() - years(5), 
                              max = Sys.Date() + years(1),
                              separator = '--', 
                              start = Sys.Date() - months(12)
                              ),
               selectizeInput('time_units', 'Select time unit for calculation', 
                           choices = c('week', 'month', 'year'), 
                           selected = 'month'),
               selectizeInput('output_units', 'Select output unit for calculation', 
                              choices = list('Flowcells' = 'fc', 'Bases' = 'sum_bases', 'Reads' = 'sum_reads'), 
                              selected = 'fc'),
               checkboxInput('ont_output_type', 'Cumulative output', value = FALSE)
               ),
             box(
               width = 9,
               highchartOutput('output_graph')
                 )
             ),
           fluidRow(
             box(width = 12, 
                 dataTableOutput('output_table')
                 )
           )
          ),
  tabPanel('PacBio usage'),
  tabPanel('PacBio output')
    )
  )
)

server <- function(input, output, session) {
  # reactives
  
  dfr <- reactive({
    df[df$start >= input$dates[1] & df$end <= input$dates[2], ]
  })
  
  dfr2 <- reactive({
    df[df$start >= input$dates2[1] & df$end <= input$dates2[2], ]
  })
  
  dfr_merged <- reactive({
    dfmerged[dfmerged$start <= input$dates[2] & dfmerged$end >= input$dates[1], ]
  })
  
  dfr_merged_total <- reactive({
    dfmerged_total[dfmerged_total$start <= input$dates[2] & dfmerged_total$end >= input$dates[1], ]
  })
  
  ont_output_data <- reactive({
    dfr2() %>% 
      #group_by(group) %>%
      mutate(m = lubridate::floor_date(start, unit = input$time_units)) %>%
      mutate(m = as.Date(m), c = 1) %>%   # used to count flow cells 
      complete(m = seq.Date(min(m), max(m), by = input$time_units), group, fill = list(c = 0)) %>%
      group_by(m, group) %>%
      reframe(fc = sum(c, na.rm = TRUE), # set count to 0 if no bases produced
              sum_reads = sum(reads_pass, na.rm = TRUE), 
              sum_bases = sum(bases_pass, na.rm = TRUE)) %>%
      group_by(group) %>%
      mutate(cum_reads = cumsum(sum_reads), cum_bases = cumsum(sum_bases), cum_fc = cumsum(fc))
  })
  
  # outputs
  output$usage_timevis <- renderTimevis({
    mydata <- df %>% mutate(style = .data[[input$color]])
    timevis(mydata, 
            groups = groups_df, fit = F,
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
  
  output$selected_dates <- renderPrint({
    tags$p(paste0(input$dates[1], ' - ', input$dates[2])
           )
  })
  
  # output$download <- downloadHandler(
  #   filename = function() {
  #     paste0('ontusage-', Sys.Date(), '.csv')
  #   },
  #   content = function(file) {
  #     write.csv(df, file, row.names = F)
  #   }
  # )
  
  output$datatable <- renderDataTable({
    mydata <- 
      dfr() %>%
      #dplyr::filter(sample_id %in% input$sampleids) %>%
      mutate(start_date = as.Date(start)) %>%
      dplyr::select(c('start_date', 'flowcell', 'group', 'sample_id', 'title', 'mean_qscore'))

    datatable(mydata, 
              caption = paste0('ONT usage raw data from ',input$dates[1], ' to ', input$dates[2], '.' ),
              rownames = FALSE,
              extensions = 'Buttons',
              filter = 'top', 
              selection = 'single', 
              class = 'hover',
              options = list(
                buttons = c('copy', 'csv', 'excel'),
                searchHighlight = TRUE,
                pageLength = 50,
                autoWidth = TRUE, 
                dom = 'Btp')
              )
  })
  
  output$output_graph <- renderHighchart({
      
    highchart() %>%
      hc_add_series(data = ont_output_data(), 
                    type = if_else(input$ont_output_type, 'line', 'column'), 
                    hcaes_string(x = 'm', y = input$output_units, group = 'group')
                    ) %>%
      hc_xAxis(type = 'datetime') %>%
      hc_yAxis(title = list(text = input$output_units)) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_title(text = paste0('Output per ', input$time_units, ' and platform')) %>%
      hc_subtitle(text = paste0('Data from ', input$dates2[1], ' to ', input$dates2[2])) %>%
      hc_caption(text = paste0(
        'For selected time range: ',
        sum(ont_output_data()$fc, na.rm = T), ' flowcells, ',
        siformat(sum(ont_output_data()$sum_reads, na.rm = T)), ' reads, ',
        siformat(sum(ont_output_data()$sum_bases, na.rm = T)), ' bases. Only pass reads/bases are considered.'
      )
                 )
  })
  
  output$output_table <- renderDataTable({
    datatable(ont_output_data(),
              caption = paste0('ONT output raw data from ',
                               input$dates2[1],' to ', input$dates2[2], 
                               ', aggregated by ', input$time_units, '.'),
              extensions = 'Buttons',
              rownames = FALSE, 
              filter = 'top', 
              selection = 'single', 
              class = 'hover', 
              options = list(
                buttons = c('copy', 'csv', 'excel'),
                searchHighlight = TRUE,
                autoWidth = TRUE, 
                #lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                info = FALSE,
                pageLength = 50,
                dom = 'Btp' #https://datatables.net/reference/option/dom
              )
              )
  })
    
  # OBSERVERS
  
  # change ONT graph output to cumulative
  observeEvent(input$ont_output_type, {
      updateSelectizeInput(
        session, 'output_units',
        choices = if(input$ont_output_type == FALSE) {
          list('Flowcells' = 'fc', 'Bases' = 'sum_bases', 'Reads' = 'sum_reads')
          } else {
            list('Flowcells' = 'cum_fc', 'Bases' = 'cum_bases', 'Reads' = 'cum_reads')
            },
        selected = if(input$ont_output_type == FALSE) {'fc'} else {'cum_fc'})
  }, ignoreInit = TRUE, )
  
  observe({
    mydate <- dfr()[input$datatable_rows_selected, ]$start
    myitemid <- dfr()[input$datatable_rows_selected, ]$id
    #tv_selected <- input$usage_timevis_selected
    
    timevis::centerTime('usage_timevis', mydate) %>%
      timevis::setSelection(itemId = myitemid)
    #proxy %>% selectRows( which(dfr()$id == tv_selected) )
  })
  
}

shinyApp(ui = ui, server = server)
