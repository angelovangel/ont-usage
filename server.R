
server <- function(input, output, session) {

  #### REACTIVE DATA
  
  dfr_module <- callModule(
    module = selectizeGroupServer,
    id = 'ont-usage-filters', data = df, vars = c('division', 'pi_name')
  )
  
  dfr <- reactive({
    dfr_module()[dfr_module()$start >= input$dates[1] & dfr_module()$end <= input$dates[2], ]
  })
  
  dfr2_module <- callModule(
    module = selectizeGroupServer, 
    id = 'ont-output-filters', data = df, vars = c('division', 'pi_name')
  )
  
  dfr2 <- reactive({
    dfr2_module()[dfr2_module()$start >= input$dates2[1] & dfr2_module()$end <= input$dates2[2], ]
  })
  
  dfr_merged <- reactive({
    dfr() %>%
      merge_overlaps(start, end, group) %>% 
      mutate(group = factor(group))
  })
  
  dfr_merged_total <- reactive({
    dfr_merged() %>%
      reframe(total_rng = iv_groups(rng)) %>% 
      mutate(start = vctrs::field(total_rng, 1),
             end = vctrs::field(total_rng, 2),
             dur = end - start)
  })
  
  ont_output_data <- reactive({
    dfr2() %>% 
      mutate( group = .data[[input$ont_output_groupby]] ) %>%
      dplyr::filter(!is.na(group)) %>%
      mutate(m = lubridate::floor_date(start, unit = input$time_units)) %>%
      mutate(m = as.Date(m), c = 1) %>%   # used to count flow cells 
      complete(m = seq.Date(from = min(m, na.rm = T), to = max(m, na.rm = T), 
                            by = input$time_units), group, fill = list(c = 0)
      ) %>%
      group_by(m, group) %>%
      reframe(fc = sum(c, na.rm = TRUE), # set count to 0 if no bases produced
              sum_reads = sum(reads_pass, na.rm = TRUE), 
              sum_bases = sum(bases_pass, na.rm = TRUE)) %>%
      group_by(group) %>%
      mutate(cum_reads = cumsum(sum_reads), cum_bases = cumsum(sum_bases), cum_fc = cumsum(fc)) 
  })
  
  ### OUTPUTS
  
  ### ONT USAGE
  
  output$usage_timevis <- renderTimevis({
    mydata <- dfr() %>% mutate(style = .data[[input$color]])
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
  
  # This is below the value boxes
  output$selected_dates <- renderPrint({
    tags$p(
      paste0("The data is filtered: ",
             input$dates[1], ' - ', input$dates[2], " / ", 
             #https://stackoverflow.com/questions/63908116/viewing-selected-values-of-selectizegroup-module-parameters
             str_flatten( input[["ont-usage-filters-division"]], collapse = " / "), " / ",
             str_flatten( input[["ont-usage-filters-pi_name"]], collapse = " / ")
      )
    )
  })
  
  output$ont_runhours <- renderValueBox({
    grid_time <- sum(dfr_merged()[dfr_merged()$group == 'grid', ]$dur, na.rm = T) %>% as.duration() %>% seconds_to_period()
    prom_time <- sum(dfr_merged()[dfr_merged()$group == 'prom', ]$dur, na.rm = T) %>% as.duration() %>% seconds_to_period()
    total_time <- sum(dfr_merged_total()$dur, na.rm = T) %>% as.duration() %>% seconds_to_period()
    selected_time <- (input$dates[2] - input$dates[1]) %>% as.duration() %>% seconds_to_period()
    valueBox(
      value = tags$p(
        sprintf('%s days %s hours', day(total_time), hour(total_time)), 
        style = "font-size: 80%; font-weight:normal;"), 
      subtitle = HTML(
        paste0(
          'selected duration <b>', sprintf('%s days %s hours', day(selected_time), hour(selected_time)), '</b><br>',
          'prom <b>', sprintf('%s days %s hours', day(prom_time), hour(prom_time)), '</b><br>',
          'grid <b>', sprintf('%s days %s hours', day(grid_time), hour(grid_time)), '</b>'
        )
      ),
      color = 'green'
    )
    
  })
  
  output$ont_usage <- renderValueBox({
    grid_time <- sum(dfr_merged()[dfr_merged()$group == 'grid', ]$dur, na.rm = T) %>% as.duration()
    prom_time <- sum(dfr_merged()[dfr_merged()$group == 'prom', ]$dur, na.rm = T) %>% as.duration()
    total_time <- sum(dfr_merged_total()$dur, na.rm = T) %>% as.duration()
    selected_time <- (input$dates[2] - input$dates[1]) %>% as.duration()
    runscount <-  length(unique(dfr()$flowcell_id.x))
    
    
    total_usage <- paste0(round(total_time/selected_time*100, 0), ' % usage')
    grid_usage <- paste0(round(grid_time/selected_time*100, 0), ' %')
    prom_usage <- paste0(round(prom_time/selected_time*100, 0), ' %')
    
    valueBox(value = tags$p(total_usage, style = "font-size: 80%; font-weight:normal;"),
             subtitle = HTML(paste0(
               '<b>', runscount, '</b> runs', '<br>',
               'prom <b>' , prom_usage, 
               '</b><br> grid <b>', grid_usage
             )),
             color = 'green')
    
  })
  
  output$datatable <- renderDataTable({
    mydata <- 
      dfr() %>%
      #dplyr::filter(sample_id %in% input$sampleids) %>%
      mutate(start_date = as.Date(start)) %>%
      dplyr::select(c('start_date', 'id', 'flowcell_id.x','pi_name', 'division', 'group', 'sample_id', 
                      'reads_pass', 'bases_pass', 'mean_qscore', 'nx_pass'))
    
    datatable(mydata, 
              caption = paste0('ONT usage raw data from ',input$dates[1], ' to ', input$dates[2], '.' ),
              rownames = FALSE,
              extensions = 'Buttons',
              filter = 'top', 
              selection = 'single', 
              class = 'hover',
              options = list(
                columnDefs = list(list(visible = FALSE, targets = 'id')),
                buttons = c('copy', 'csv', 'excel'),
                searchHighlight = TRUE,
                pageLength = 500,
                autoWidth = TRUE, 
                dom = 'Btp')
    )
  })
  
  ### ONT OUTPUT
  output$ont_flowcells <- renderValueBox({
    cellcounts <- dfr2() %>% group_by(group) %>% 
      summarise(flowcell = length(unique(content)))
    myvalue <- paste0(sum(cellcounts$flowcell, na.rm = T), ' flowcells')
    valueBox(
      color = 'green',
      value = tags$p(myvalue, style = "font-size: 80%; font-weight:normal;"),
      subtitle = HTML(paste0(
        'prom <b>', cellcounts$flowcell[cellcounts$group == 'prom'], '</b> <br>',
        'grid <b>', cellcounts$flowcell[cellcounts$group == 'grid']
      ))
    )
  })
  
  output$ont_output <- renderValueBox({
    promoutput <- sum(dfr2()[dfr2()$group == 'prom', ]$bases_pass, na.rm = T)
    gridoutput <- sum(dfr2()[dfr2()$group == 'grid', ]$bases_pass, na.rm = T)
    totaloutput <- sum(dfr2()$bases_pass, na.rm = T)
    #print(selected)
    myvalue <- paste0(siformat(totaloutput), ' bases')
    mysubtitle <- HTML(paste0('prom <b>', siformat(promoutput) , '</b> <br>',
                              'grid <b>', siformat(gridoutput), ' </b> '
    ))
    #print(mysubtitle)
    valueBox(
      color = 'green',
      value = tags$p(myvalue, style = "font-size: 80%; font-weight:normal;"),
      subtitle = mysubtitle,
    )
  })
  
  output$output_graph <- renderHighchart({
    validate(
      need(nrow(dfr2()) >= 1, 'No data!')
    )
    
    highchart() %>%
      hc_add_series(data = ont_output_data(), 
                    type = if_else(input$ont_output_type, 'line', 'column'), 
                    hcaes_string(
                      x = 'm', 
                      y = input$output_units,
                      group = 'group')
      ) %>%
      hc_xAxis(type = 'datetime') %>%
      hc_yAxis(title = list(text = input$output_units)) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_title(text = paste0('Output per ', input$time_units, ' and ', input$ont_output_groupby)) %>%
      hc_subtitle(text = paste0('Data from ', input$dates2[1], ' to ', input$dates2[2])) %>%
      hc_caption(text = paste0(
        'For selected time range: ',
        sum(ont_output_data()$fc, na.rm = T), ' runs, ',
        siformat(sum(ont_output_data()$sum_reads, na.rm = T)), ' reads, ',
        siformat(sum(ont_output_data()$sum_bases, na.rm = T)), ' bases. Only pass reads/bases are considered.')
      )
  })
  
  output$output_table <- renderDataTable({
    validate(
      need(nrow(dfr2()) >= 1, 'No data!')
    )
    
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
                pageLength = 500,
                dom = 'Btp' #https://datatables.net/reference/option/dom
              )
    )
  })
  
  ### OBSERVERS
  
  # switch ONT graph output to cumulative
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
  
  # center timevis based on selection
  observe({
    mydate <- dfr()[input$datatable_rows_selected, ]$start
    myitemid <- dfr()[input$datatable_rows_selected, ]$id
    #tv_selected <- input$usage_timevis_selected
    
    timevis::centerTime('usage_timevis', mydate) %>%
      timevis::setSelection(itemId = myitemid)
    #proxy %>% selectRows( which(dfr()$id == tv_selected) )
  })
  
}
