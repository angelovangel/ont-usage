
server <- function(input, output, session) {
  
  #==========user auth ==================================# 
  credentials <- readRDS('data/credentials.rds')
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  #==========user auth ==================================#
  
 #### LOAD DATA ####
  # data loaded once for each session
  df <- vroom('data/df.csv') %>% 
    mutate(group = factor(group), division = factor(division))
  
  groups_df <- data.frame(
    id = c('grid', 'prom'),
    content = c('GridION', 'PromethION')
  )
  
  df_pb <- vroom('data/pb_dump_processed.csv') %>%
    mutate(instrumentType = factor(instrumentType), 
           group = factor(instrumentType), 
           division = factor(division)
           )
  
  df_pb_groups <- data.frame(
    id = c('Sequel', 'Sequel2', 'Sequel2e', 'Revio'), 
    content = c('Sequel', 'Sequel2', 'Sequel2e', 'Revio')
  ) 
  
  
  
 #### REACTIVE DATA ####
  ### ONT ####
  df_ont_module <- callModule(
    module = selectizeGroupServer,
    id = 'ont-usage-filters', data = df, vars = c('division', 'pi_name')
  )
  
  dfr <- reactive({
    df_ont_module()[df_ont_module()$start >= input$dates[1] & df_ont_module()$end <= input$dates[2], ]
  })
  
  
  df2_module <- callModule(
    module = selectizeGroupServer, 
    id = 'ont-output-filters', data = df, vars = c('division', 'pi_name')
  )
  
  dfr2 <- reactive({
    df2_module()[df2_module()$start >= input$dates2[1] & df2_module()$end <= input$dates2[2], ]
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
  
  ### PB ####
  df_pb_module <- callModule(
    module = selectizeGroupServer,
    id = 'pb-usage-filters', data = df_pb, vars = c('division', 'pi_name')
  )
  
  df_pb_output_module <- callModule(
    module = selectizeGroupServer,
    id = 'pb-output-filters', data = df_pb, vars = c('division', 'pi_name')
  )
  
  dfpbr <- reactive({
    d <- df_pb_module()[df_pb_module()$run_startedAt >= input$pb_dates[1] & df_pb_module()$run_completedAt <= input$pb_dates[2], ]
    d %>%
      mutate(
        start = run_startedAt,
        end = run_completedAt, 
        content = run_name, 
        id = run_uniqueId,
        title = paste0(run_name, '<br>', totalCells, ' cells<br>', division)
      ) %>%
      filter(!is.na(start)) #& !is.na(end))
  })
  
  dfpbr_merged <- reactive({
    dfpbr() %>%
      merge_overlaps(start, end, group) %>%
      mutate(group = factor(group))
  })
  
  dfpbr_merged_total <- reactive({
    dfpbr_merged() %>%
      reframe(total_rng = iv_groups(rng)) %>% 
      mutate(start = vctrs::field(total_rng, 1),
             end = vctrs::field(total_rng, 2),
             dur = end - start)
  })
  
  dfpbr2 <- reactive({
    df_pb_output_module()[df_pb_output_module()$run_startedAt >= input$pb_dates2[1] & df_pb_output_module()$run_completedAt <= input$pb_dates2[2], ]
  })
  
  pb_output_data <- reactive({
    dfpbr2() %>% 
      #ungroup() %>%
      dplyr::filter(coll_status == 'Complete') %>%
      mutate( group = .data[[input$pb_output_groupby]] ) %>%
      dplyr::filter(!is.na(group)) %>%
      mutate(m = lubridate::floor_date(run_startedAt, unit = input$pb_time_units)) %>%
      mutate(m = as.Date(m), c = 1) %>%   # used to count flow cells 
      complete(m = seq.Date(from = min(m, na.rm = T), to = max(m, na.rm = T), 
                            by = input$pb_time_units), 
               group, fill = list(c = 0)
      ) %>%
      group_by(m, group) %>%
      reframe(fc = sum(c, na.rm = TRUE), # set count to 0 if no bases produced
              sum_ccs_reads = sum(ccs2.number_of_ccs_reads, na.rm = TRUE), 
              sum_ccs_bases = sum(ccs2.total_number_of_ccs_bases, na.rm = TRUE)) %>%
      group_by(group) %>%
      mutate(cum_ccs_reads = cumsum(sum_ccs_reads), cum_ccs_bases = cumsum(sum_ccs_bases), cum_fc = cumsum(fc)) 
  })
  
  
 ### OUTPUTS ####
 #### ONT USAGE ####
  
  output$usage_timevis <- renderTimevis({
    mydata <- dfr() %>% mutate(style = .data[[input$color]])
    timevis(mydata, 
            groups = groups_df, fit = F,
            options = list(
              #start = Sys.Date() - 30, 
              #end = Sys.Date() + 3, 
              min = Sys.Date() - years(5),
              max = Sys.Date() + years(1), 
              maxHeight = '500px', 
              #minHeight = '300px', 
              zoomMin = 604800000, zoomMax = 31556926000, 
              clickToUse = FALSE, 
              stack = input$stack
            )
    ) %>% 
      #addItems(data = dfmerged) %>%
      setSelection(itemId = last(df$id)) %>%
      setWindow(start = input$dates[1], end = input$dates[2] + days(14))
    
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
          'selected <b>', sprintf('%s days %s hours', day(selected_time), hour(selected_time)), '</b><br>',
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
  
 #### ONT OUTPUT ####
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
                    hcaes(
                      x = m, 
                      y = .data[[input$output_units]],
                      group = group)
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
  
 #### PB USAGE ####
  output$pb_usage_timevis <- renderTimevis({
    mydata <- dfpbr() %>%
      mutate(style = .data[[input$pb_usage_color]]) %>%
      dplyr::select(start, end, group, content, title, style) %>%
      unique()

    timevis(
      mydata, fit = F,
      groups = df_pb_groups,
      options = list(
        stack = input$pb_stack,
        maxHeight = '500px', 
        #minHeight = '300px', 
        zoomMin = 604800000, zoomMax = 31556926000
      )
    ) %>%
    setWindow(start = input$pb_dates[1], end = input$pb_dates[2] + days(14))
  })
  
  output$pb_runhours <- renderValueBox({
    seq2_time <- sum(dfpbr_merged()[dfpbr_merged()$group == 'Sequel2', ]$dur, na.rm = T) %>% as.duration() %>% seconds_to_period()
    seq2e_time <- sum(dfpbr_merged()[dfpbr_merged()$group == 'Sequel2e', ]$dur, na.rm = T) %>% as.duration() %>% seconds_to_period()
    revio_time <- sum(dfpbr_merged()[dfpbr_merged()$group == 'Revio', ]$dur, na.rm = T) %>% as.duration() %>% seconds_to_period()
    total_time <- sum(dfpbr_merged_total()$dur, na.rm = T) %>% as.duration() %>% seconds_to_period()
    selected_time <- (input$pb_dates[2] - input$pb_dates[1]) %>% as.duration() %>% seconds_to_period()
    valueBox(
      value = tags$p(
        sprintf('%s days %s hours', day(total_time), hour(total_time)), 
        style = "font-size: 80%; font-weight:normal;"), 
      subtitle = HTML(
        paste0(
          'selected <b>', sprintf('%s days %s hours', day(selected_time), hour(selected_time)), '</b><br>',
          'Seq2 <b>', sprintf('%s days %s hours', day(seq2_time), hour(seq2_time)), '</b><br>',
          'Seq2e <b>', sprintf('%s days %s hours', day(seq2e_time), hour(seq2e_time)), '</b><br>',
          'Revio <b>', sprintf('%s days %s hours', day(revio_time), hour(revio_time)), '</b>'
        )
      ),
      color = 'green'
    )
  })
  
  output$pb_usage <- renderValueBox({
    seq2_time <- sum(dfpbr_merged()[dfpbr_merged()$group == 'Sequel2', ]$dur, na.rm = T) %>% as.duration() %>% seconds_to_period()
    seq2e_time <- sum(dfpbr_merged()[dfpbr_merged()$group == 'Sequel2e', ]$dur, na.rm = T) %>% as.duration() %>% seconds_to_period()
    revio_time <- sum(dfpbr_merged()[dfpbr_merged()$group == 'Revio', ]$dur, na.rm = T) %>% as.duration() %>% seconds_to_period()
    total_time <- sum(dfpbr_merged_total()$dur, na.rm = T) %>% as.duration() %>% seconds_to_period()
    selected_time <- (input$pb_dates[2] - input$pb_dates[1]) %>% as.duration() %>% seconds_to_period()
    
    total_usage <- paste0(round(total_time/selected_time*100, 0), ' % usage')
    seq2_usage <- paste0(round(seq2_time/selected_time*100, 0), ' %')
    seq2e_usage <- paste0(round(seq2e_time/selected_time*100, 0), ' %')
    revio_usage <- paste0(round(revio_time/selected_time*100, 0), ' %')
    runscount <- length(dfpbr()$run_uniqueId %>% unique())
    cellscount <- length(dfpbr()$coll_uniqueId %>% unique())
    
    valueBox(value = tags$p(total_usage, style = "font-size: 80%; font-weight:normal;"),
             subtitle = HTML(paste0(
               '<b>', runscount, '</b> runs <b>', cellscount, '</b> cells <br>',
               'Seq2 <b>' , seq2_usage, '</b><br>',
               'Seq2e <b>', seq2e_usage,'</b><br>',
               'Revio <b>', revio_usage
             )),
             color = 'green')
  })
  
  output$pb_usage_datatable <- renderDataTable({
    mydata <- 
      dfpbr() %>%
      #dplyr::filter(sample_id %in% input$sampleids) %>%
      mutate(start_date = as.Date(start)) %>%
      arrange(desc(start_date)) %>%
      dplyr::select(c('start_date', 'group', 'run_name', 'coll_name','run_status', 'division', 'pi_name', 
                      'ccs2.number_of_ccs_reads', 'ccs2.total_number_of_ccs_bases')
                    )
    datatable(mydata, 
              caption = paste0('PacBio usage raw data from ',input$pb_dates[1], ' to ', input$pb_dates[2], '.' ),
              rownames = FALSE,
              extensions = 'Buttons',
              filter = 'top', 
              selection = 'single', 
              class = 'hover',
              options = list(
                #columnDefs = list(list(visible = FALSE, targets = 'id')),
                buttons = c('copy', 'csv', 'excel'),
                searchHighlight = TRUE,
                pageLength = 500,
                autoWidth = TRUE, 
                dom = 'Btp')
    )
  })
 #### PB OUTPUT ####
  output$pb_flowcells <- renderValueBox({
    cellcounts <- dfpbr2() %>%
      ungroup() %>%
      dplyr::filter(coll_status == 'Complete') %>%
      group_by(instrumentType) %>% 
      summarise(smrtcells = length(unique(coll_uniqueId)))
    
    myvalue <- paste0(sum(cellcounts$smrtcells, na.rm = T), ' smrtcells')
    valueBox(
      color = 'green',
      value = tags$p(myvalue, style = "font-size: 80%; font-weight:normal;"),
      subtitle = HTML(paste0(
        'Sequel <b>', cellcounts$smrtcells[cellcounts$instrumentType == 'Sequel'], '</b> <br>',
        'Sequel2 <b>', cellcounts$smrtcells[cellcounts$instrumentType == 'Sequel2'], '</b> <br>',
        'Sequel2e <b>', cellcounts$smrtcells[cellcounts$instrumentType == 'Sequel2e'], '</b> <br>',
        'Revio <b>', cellcounts$smrtcells[cellcounts$instrumentType == 'Revio']
      ))
    )
  })
  
  output$pb_output <- renderValueBox({
    seq_output <- sum(dfpbr2()[dfpbr2()$instrumentType == 'Sequel', ]$ccs2.total_number_of_ccs_bases, na.rm = T)
    seq2_output <- sum(dfpbr2()[dfpbr2()$instrumentType == 'Sequel2', ]$ccs2.total_number_of_ccs_bases, na.rm = T)
    seq2e_output <- sum(dfpbr2()[dfpbr2()$instrumentType == 'Sequel2e', ]$ccs2.total_number_of_ccs_bases, na.rm = T)
    revio_output <- sum(dfpbr2()[dfpbr2()$instrumentType == 'Revio', ]$ccs2.total_number_of_ccs_bases, na.rm = T)
    total_output <- sum(dfpbr2()$ccs2.total_number_of_ccs_bases, na.rm = T)
    #print(selected)
    myvalue <- paste0(siformat(total_output), ' bases')
    mysubtitle <- HTML(paste0('Seq <b>', siformat(seq_output) , '</b> <br>',
                              'Seq2 <b>', siformat(seq2_output) , '</b> <br>',
                              'Seq2e <b>', siformat(seq2e_output) , '</b> <br>',
                              'Revio <b>', siformat(revio_output) , '</b> <br>') 
                       )
    #print(mysubtitle)
    valueBox(
      color = 'green',
      value = tags$p(myvalue, style = "font-size: 80%; font-weight:normal;"),
      subtitle = mysubtitle,
    )
  })
  
  output$pb_output_graph <- renderHighchart({
    
    validate(
      need(nrow(dfpbr2()) >= 1, 'No data!')
    )
    highchart() %>%
      hc_add_series(data = pb_output_data(), 
                    type = if_else(input$pb_output_type, 'line', 'column'), 
                    hcaes(
                      x = m, 
                      y = .data[[input$pb_output_units]],
                      group = group)
      ) %>%
      hc_xAxis(type = 'datetime') %>%
      hc_yAxis(title = list(text = input$pb_output_units)) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_title(text = paste0('Output per ', input$pb_time_units, ' and ', input$pb_output_groupby)) %>%
      hc_subtitle(text = paste0('Data from ', input$pb_dates2[1], ' to ', input$pb_dates2[2])) #%>%
      # hc_caption(text = paste0(
      #   'For selected time range: ',
      #   sum(pb_output_data()$fc, na.rm = T), ' runs, ',
      #   siformat(sum(ont_output_data()$sum_reads, na.rm = T)), ' reads, ',
      #   siformat(sum(ont_output_data()$sum_bases, na.rm = T)), ' bases. Only pass reads/bases are considered.')
      # )
  })
  
  output$pb_output_table <- renderDataTable({
    validate(
      need(nrow(dfpbr2()) >= 1, 'No data!')
    )
    
    datatable(pb_output_data(),
              caption = paste0('PacBio output raw data from ',
                               input$pb_dates2[1],' to ', input$pb_dates2[2], 
                               ', aggregated by ', input$pb_time_units, '.'),
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
  
 #### OBSERVERS ####
  
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
  }, ignoreInit = TRUE)
  
  # switch PB graph output to cumulative
  observeEvent(input$pb_output_type, {
    updateSelectizeInput(
      session, 'pb_output_units',
      choices = if(input$pb_output_type == FALSE) {
        list('Flowcells' = 'fc', 'Bases' = 'sum_ccs_bases', 'Reads' = 'sum_ccs_reads')
      } else {
        list('Flowcells' = 'cum_fc', 'Bases' = 'cum_ccs_bases', 'Reads' = 'cum_ccs_reads')
      },
      selected = if(input$pb_output_type == FALSE) {'fc'} else {'cum_fc'})
  }, ignoreInit = TRUE)
  
  # center ONT timevis based on selection
  observe({
    mydate <- dfr()[input$datatable_rows_selected, ]$start
    myitemid <- dfr()[input$datatable_rows_selected, ]$id
    #tv_selected <- input$usage_timevis_selected
    
    timevis::centerTime('usage_timevis', mydate) %>%
      timevis::setSelection(itemId = myitemid)
    #proxy %>% selectRows( which(dfr()$id == tv_selected) )
  })
  
  # center PB timevis based on selection
  observe({
    mydate <- dfpbr()[input$pb_usage_datatable_rows_selected, ]$start
    #myitemid <- dfpbr()[input$pb_usage_datatable_rows_selected, ]$id
    #tv_selected <- input$usage_timevis_selected
    
    timevis::centerTime('pb_usage_timevis', mydate) #%>%
      #timevis::setSelection(itemId = myitemid)
    #proxy %>% selectRows( which(dfr()$id == tv_selected) )
  })
  
}
