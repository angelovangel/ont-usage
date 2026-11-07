
server <- function(input, output, session) {
  
  #==========user auth ==================================# 
  credentials <- readRDS('config/credentials.rds')
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  #==========user auth ==================================#
  
 #### LOAD DATA ####
  # data loaded once for each session
  df <- vroom('data/app/df.csv') %>% 
    mutate(group = factor(group), division = factor(division)) %>% 
    arrange(desc(start))
  
  groups_df <- data.frame(
    id = c('grid', 'prom'),
    content = c('GridION', 'PromethION')
  )
  
  df_pb <- vroom('data/app/pb_dump_processed.csv') %>%
    mutate(instrumentType = factor(instrumentType), 
           group = factor(instrumentType), 
           division = factor(division)
           )
  
  df_pb_groups <- data.frame(
    id = c('Sequel', 'Sequel2', 'Sequel2e', 'Revio'), 
    content = c('Sequel', 'Sequel2', 'Sequel2e', 'Revio'),
    serial = c(NA, '64068', '64468e', '84180')
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
    d <- dfr2() %>%
      mutate( group = .data[[input$ont_output_groupby]] ) %>%
      dplyr::filter(!is.na(group))
    validate(
      need(nrow(d) >= 1, 'No data!')
    )
    d %>%
      mutate(m = lubridate::floor_date(start, unit = input$time_units)) %>%
      mutate(m = as.Date(m)) %>%
      # used to count flow cells - c_new counts each flow cell (content) only once, at its first use
      # c_all counts it once in each time unit it was used
      arrange(start) %>%
      group_by(group) %>%
      mutate(c_new = as.numeric(!duplicated(content))) %>%
      group_by(group, m) %>%
      mutate(c_all = as.numeric(!duplicated(content))) %>%
      ungroup() %>%
      complete(m = seq.Date(from = min(m, na.rm = T), to = max(m, na.rm = T),
                            by = input$time_units), group, fill = list(c_new = 0, c_all = 0)
      ) %>%
      group_by(m, group) %>%
      reframe(fc_new = sum(c_new, na.rm = TRUE), # set count to 0 if no bases produced
              fc = if (input$ont_fc_count == 'new') fc_new else sum(c_all, na.rm = TRUE),
              sum_reads = sum(reads_pass, na.rm = TRUE),
              sum_bases = sum(bases_pass, na.rm = TRUE)) %>%
      group_by(group) %>%
      # cumulative is always new flow cells, otherwise re-used ones are counted again
      mutate(cum_reads = cumsum(sum_reads), cum_bases = cumsum(sum_bases), cum_fc = cumsum(fc_new))
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
    d <- df_pb_module()[df_pb_module()$run_startedAt >= input$pb_dates[1] & df_pb_module()$run_completedAt <= input$pb_dates[2] | is.na(df_pb_module()$run_completedAt), ]
    d %>%
      dplyr::filter(run_status == 'Running' | run_status == 'Complete') %>%
      mutate(
        start = run_startedAt,
        end = if_else(run_status == 'Running', Sys.time(), run_completedAt), 
        content = if_else(run_status == 'Running', 'Running', run_name),
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
    # running runs (no run_completedAt) are excluded, otherwise they give all-NA rows
    d <- df_pb_output_module()
    d[!is.na(d$run_completedAt) & d$run_startedAt >= input$pb_dates2[1] & d$run_completedAt <= input$pb_dates2[2], ]
  })
  
  pb_output_data <- reactive({
    d <- dfpbr2() %>%
      #ungroup() %>%
      dplyr::filter(coll_status == 'Complete') %>%
      mutate( group = .data[[input$pb_output_groupby]] ) %>%
      dplyr::filter(!is.na(group))
    validate(
      need(nrow(d) >= 1, 'No data!')
    )
    d %>%
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
  
  smrt_token <- reactive({
    # valid for 20 minutes
    invalidateLater(1200000)
    smrtlinker::smrt_token(
      baseurl = Sys.getenv("SMRT_BASE"), user = Sys.getenv("SMRT_USER"), pass = Sys.getenv("SMRT_PASS")
      ) %>% .$access_token
  })
  
  pb_status <- reactive({
    validate(
      need(
        !is.null(curl::nslookup('smrt.ibex.kaust.edu.sa', error = F)), 'No connection to SMRTLink'
      )
    )
    invalidateLater(30000, session)
    smrtlinker::smrt_state(baseurl = Sys.getenv("SMRT_BASE"), token = smrt_token())
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
      value = vb_value(sprintf('%s days %s hours', day(total_time), hour(total_time))),
      subtitle = vb_table(tibble(
        group = c('prom', 'grid', 'total', 'selected'),
        Time = fmt_dh(c(prom_time, grid_time, total_time, selected_time))
      )),
      color = 'green'
    )
    
  })
  
  output$ont_usage <- renderValueBox({
    grid_time <- sum(dfr_merged()[dfr_merged()$group == 'grid', ]$dur, na.rm = T) %>% as.duration()
    prom_time <- sum(dfr_merged()[dfr_merged()$group == 'prom', ]$dur, na.rm = T) %>% as.duration()
    total_time <- sum(dfr_merged_total()$dur, na.rm = T) %>% as.duration()
    selected_time <- (input$dates[2] - input$dates[1]) %>% as.duration()
    runs <- function(g) {n_distinct(dfr()$flowcell_id[dfr()$group %in% g])}

    total_usage <- paste0(round(total_time/selected_time*100, 0), ' % usage')

    valueBox(value = vb_value(total_usage),
             subtitle = vb_table(tibble(
               group = c('prom', 'grid', 'total'),
               Usage = paste0(round(c(prom_time, grid_time, total_time)/selected_time*100, 0), ' %'),
               Runs = c(runs('prom'), runs('grid'), n_distinct(dfr()$flowcell_id))
             )),
             color = 'green')
    
  })
  
  output$datatable <- renderDataTable({
    mydata <- 
      dfr() %>%
      #dplyr::filter(sample_id %in% input$sampleids) %>%
      mutate(start_date = as.Date(start)) %>%
      dplyr::select(c('start_date', 'id', 'flowcell_id','pi_name', 'division', 'group', 'sample_id', 
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
                scrollX = TRUE,
                dom = 'Btp')
    )
  })
  
 #### ONT OUTPUT ####
  output$ont_flowcells <- renderValueBox({
    # flowcells are unique (re-used ones counted once), runs are all runs
    cellcounts <- tibble(group = c('prom', 'grid')) %>%
      left_join(
        dfr2() %>% group_by(group) %>% summarise(Flowcells = n_distinct(content), Runs = n()),
        by = 'group'
      ) %>%
      mutate(across(-group, ~ coalesce(.x, 0L))) %>%
      bind_rows(tibble(group = 'total', Flowcells = n_distinct(dfr2()$content), Runs = nrow(dfr2())))
    myvalue <- paste0(n_distinct(dfr2()$content), ' flowcells')
    valueBox(
      color = 'green',
      value = vb_value(myvalue),
      subtitle = vb_table(cellcounts)
    )
  })
  
  output$ont_output <- renderValueBox({
    # pass bases/reads per platform, last is total
    groups <- list('prom', 'grid', unique(dfr2()$group))
    output_sum <- function(col) {
      sapply(groups, function(g) {sum(dfr2()[[col]][dfr2()$group %in% g], na.rm = T)})
    }
    bases <- output_sum('bases_pass')
    #print(selected)
    myvalue <- paste0(siformat(last(bases)), ' bases')
    mysubtitle <- vb_table(tibble(
      group = c('prom', 'grid', 'total'),
      Bases = sapply(bases, siformat),
      Reads = sapply(output_sum('reads_pass'), siformat)
    ))
    #print(mysubtitle)
    valueBox(
      color = 'green',
      value = vb_value(myvalue),
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
        nrow(dfr2()), ' runs, ',
        sum(ont_output_data()$fc_new, na.rm = T), ' flowcells, ',
        siformat(sum(ont_output_data()$sum_reads, na.rm = T)), ' reads, ',
        siformat(sum(ont_output_data()$sum_bases, na.rm = T)), ' bases. Only pass reads/bases are considered.')
      )
  })
  
  # how many flow cells were used once, twice, ...
  output$ont_reuse_graph <- renderHighchart({
    validate(
      need(nrow(dfr2()) >= 1, 'No data!')
    )
    reuse <- dfr2() %>%
      dplyr::filter(!is.na(content)) %>%
      count(group, content, name = 'runs') %>%
      count(group, runs, name = 'flowcells') %>%
      complete(group, runs, fill = list(flowcells = 0)) %>%
      arrange(runs) %>%
      mutate(runs = paste0(runs, 'x'))
    reused <- reuse %>%
      group_by(group) %>%
      summarise(txt = paste0(group[1], ' ', sum(flowcells[runs != '1x']), '/', sum(flowcells)))

    highchart() %>%
      hc_add_series(data = reuse, type = 'column',
                    hcaes(x = runs, y = flowcells, group = group)) %>%
      hc_xAxis(type = 'category', title = list(text = 'Runs per flowcell')) %>%
      hc_yAxis(title = list(text = 'Flowcells')) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_title(text = 'Flowcell re-use', style = list(fontSize = '14px')) %>%
      hc_subtitle(text = paste0('re-used: ', str_flatten(reused$txt, collapse = ', '))) %>%
      # click on legend items to show/hide platforms
      hc_legend(enabled = TRUE, align = 'right', verticalAlign = 'top', layout = 'vertical', floating = TRUE)
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
                scrollX = TRUE,
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
      value = vb_value(sprintf('%s days %s hours', day(total_time), hour(total_time))),
      subtitle = vb_table(tibble(
        group = c('Seq2', 'Seq2e', 'Revio', 'total', 'selected'),
        Time = fmt_dh(c(seq2_time, seq2e_time, revio_time, total_time, selected_time))
      )),
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
    groups <- list('Sequel2', 'Sequel2e', 'Revio', unique(dfpbr()$group)) # last is total
    runs <- sapply(groups, function(g) {n_distinct(dfpbr()$run_uniqueId[dfpbr()$group %in% g])})
    cells <- sapply(groups, function(g) {n_distinct(dfpbr()$coll_uniqueId[dfpbr()$group %in% g])})

    valueBox(value = vb_value(total_usage),
             subtitle = vb_table(tibble(
               group = c('Seq2', 'Seq2e', 'Revio', 'total'),
               Usage = paste0(round(c(seq2_time, seq2e_time, revio_time, total_time)/selected_time*100, 0), ' %'),
               Runs = runs,
               Cells = cells
             )),
             color = 'green')
  })
  
  output$pb_usage_datatable <- renderDataTable({
    mydata <- 
      dfpbr() %>%
      #dplyr::filter(sample_id %in% input$sampleids) %>%
      mutate(start_date = as.Date(start)) %>%
      arrange(desc(start_date)) %>%
      dplyr::select(c('start_date', 'group', 'run_name', 'coll_name','run_status', 'numCellsCompleted', 'division', 'pi_name')
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
                scrollX = TRUE,
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
    instruments <- c('Sequel', 'Sequel2', 'Sequel2e', 'Revio')
    valueBox(
      color = 'green',
      value = vb_value(myvalue),
      subtitle = vb_table(tibble(
        group = c('Seq', 'Seq2', 'Seq2e', 'Revio', 'total'),
        `SMRT cells` = c(
          sapply(instruments, function(x) {sum(cellcounts$smrtcells[cellcounts$instrumentType == x])}),
          sum(cellcounts$smrtcells, na.rm = T)
        )
      ))
    )
  })
  
  output$pb_output <- renderValueBox({
    # ccs bases/reads per instrument, last is total
    groups <- list('Sequel', 'Sequel2', 'Sequel2e', 'Revio', unique(dfpbr2()$instrumentType))
    output_sum <- function(col) {
      sapply(groups, function(g) {sum(dfpbr2()[[col]][dfpbr2()$instrumentType %in% g], na.rm = T)})
    }
    bases <- output_sum('ccs2.total_number_of_ccs_bases')
    #print(selected)
    myvalue <- paste0(siformat(last(bases)), ' bases')
    mysubtitle <- vb_table(tibble(
      group = c('Seq', 'Seq2', 'Seq2e', 'Revio', 'total'),
      Bases = sapply(bases, siformat),
      Reads = sapply(output_sum('ccs2.number_of_ccs_reads'), siformat)
    ))
    #print(mysubtitle)
    valueBox(
      color = 'green',
      value = vb_value(myvalue),
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
                scrollX = TRUE,
                #lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
                info = FALSE,
                pageLength = 500,
                dom = 'Btp' #https://datatables.net/reference/option/dom
              )
    )
  })
  
 #### PB STATUS ####
  
  output$pb_status <- renderUI({
    #print(pb_status())
    mydata <- pb_status() %>%
      left_join(df_pb_groups) %>% mutate(instrumentName = content) %>%
      mutate(numcells = if_else(numcells == 0, 1, numcells)) %>%
      mutate(total_time = as.numeric(as.duration(projectedEnd - startedAt)), 
             elapsed_frac = (1-secondsRemaining/total_time)*100 ) %>%
      mutate(elapsed_frac = if_else(is.na(elapsed_frac), 0, elapsed_frac))
    
    tagList(
      lapply(1:nrow(mydata), function(x) {
        fluidRow(
          column(width = 2, 
                 tags$div(style='text-align:center; vertical-align: middle; font-weight:bold; color:green; font-size: 1.2em;', 
                        paste0(mydata[x, ]$instrumentName, ' | ', mydata[x, ]$status)
                        )
                 ),
          column(#width = mydata[x, ]$numcells, 
                 width= 8,
                 progressBar(id = paste0(mydata[x, ]$serial), 
                             display_pct = T,
                             title = if_else(
                               mydata[x,]$status == 'Running',
                               paste0(
                                 mydata[x, ]$numcells, ' cells | ',
                                 'Start: ', format(mydata[x,]$startedAt, '%Y-%m-%d %H:%M'),
                                 ' | End: ', format(mydata[x,]$projectedEnd, '%Y-%m-%d %H:%M'), 
                                 ' | ', mydata[x, ]$cells_status
                               #' Elapsed: ', as.duration(mydata[x,]$secondsRemaining)
                               ),
                               mydata[x,]$status
                               ),
                             status = 'success', striped = T,
                             value = mydata[x,]$elapsed_frac))
          )
        }
      ),
      tags$p(style='text-align:right; color:grey;', 
             paste0('Last update: ', format(mydata[nrow(mydata), ]$timestamp, '%Y-%m-%d %H:%M:%S'))
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
