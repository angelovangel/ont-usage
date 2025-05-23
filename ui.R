ui <- dashboardPage(
  skin = 'green',
  header = dashboardHeader(title = 'TGS machine usage (BCL)', titleWidth = 350,
                           dropdownMenu(type = 'notifications', 
                                        notificationItem(
                                          text = paste0('ONT data update: ', file.mtime('data/df.csv'))),
                                        notificationItem(
                                          text = paste0('PB data update: ', file.mtime('data/pb_dump.csv')))
                           )
  ),
  sidebar = dashboardSidebar(disable = T), 
  body = dashboardBody(
    tabsetPanel(
      tabPanel(title = "ONT usage",
               
               fluidRow(
                 box(width = 3, 
                     dateRangeInput('dates',
                                    'Select interval for ONT usage calculation', 
                                    min = Sys.Date() - years(5), 
                                    max = Sys.Date() + years(1),
                                    separator = '--', 
                                    start = Sys.Date() - months(6), 
                                    end = Sys.Date() + days(1)),
                     
                     checkboxInput('stack', 'Expand items', value = FALSE),
                     radioButtons('color', 'Color flowcells by', inline = T,
                                  choiceNames = c('Q-score', 'N50','Failed bases %'),
                                  choiceValues = c('style_qscore', 'style_nx', 'style_failed'), selected = 'style_failed')
                     # these match the df.csv columns
                 ),
                 box(width = 4, 
                     selectizeGroupUI(
                       id = 'ont-usage-filters', 
                       label = 'Filter data by Division and PI',
                       params = list(
                         division = list(inputId = 'division', title = 'Division'),
                         pi_name = list(inputId = 'pi_name', title = 'PI name')
                       )
                     )
                 ),
                 
                 box(width = 5, 
                     #valueBoxOutput('output'), 
                     fluidRow(
                       valueBoxOutput('ont_runhours', width = 6), 
                       valueBoxOutput('ont_usage', width = 6)
                     ),
                     htmlOutput('selected_dates')
                 ),
                 box(width = 12,
                     timevisOutput('usage_timevis')
                     #downloadButton('download', 'Download data'),
                 ),
                 box(width = 12,
                     dataTableOutput('datatable')
                 )
               )
      ),
      tabPanel('ONT output',
               fluidRow(
                 box(
                   width = 3, 
                   dateRangeInput('dates2', 'Select interval for ONT output calculation', 
                                  min = Sys.Date() - years(5), 
                                  max = Sys.Date() + years(1),
                                  separator = '--', 
                                  start = Sys.Date() - months(12)
                   ),
                   
                   selectizeGroupUI(
                     id = 'ont-output-filters', 
                     #label = 'Filter data by Division and PI',
                     params = list(
                       division = list(inputId = 'division', title = 'Division'),
                       pi_name = list(inputId = 'pi_name', title = 'PI name')
                     )
                   ),
                   selectizeInput('time_units', 'Select time unit to aggregate data',
                                  choices = c('week', 'month', 'year'),
                                  selected = 'month'),
                   selectizeInput('output_units', 'Select output unit',
                                  choices = list('Flowcells' = 'fc', 'Bases' = 'sum_bases', 'Reads' = 'sum_reads'),
                                  selected = 'fc'),
                   selectizeInput('ont_output_groupby', 'Group data by',
                                  choices = c('Platform' = 'group', 'Division' = 'division'), 
                                  selected = 'platform'),
                   checkboxInput('ont_output_type', 'Cumulative output', value = FALSE)
                 ),
                 
                 box(width = 3, 
                     valueBoxOutput('ont_flowcells', width = 12),
                     valueBoxOutput('ont_output', width = 12)),
                 box(
                   width = 6,
                   highchartOutput('output_graph')
                 )
               ),
               fluidRow(
                 box(width = 12, 
                     dataTableOutput('output_table')
                 )
               )
      ),
      tabPanel('PacBio usage', 
               fluidRow(
                box(width = 3,
                  dateRangeInput('pb_dates', 'Select interval for PacBio usage calculation', 
                                 min = Sys.Date() - years(10), 
                                 max = Sys.Date() + years(1),
                                 separator = '--', 
                                 #start = lubridate::floor_date(Sys.Date(), 'year')), # give start of current year
                                 start = Sys.Date() - months(6), 
                                 end = Sys.Date() + days(1)
                                 ),
                  radioButtons('pb_usage_color', 
                               'Color cells by', inline = T,
                               choiceNames = c('Division', 'Number of cells'),
                               choiceValues = c('style_division', 'style_num_cells'), 
                               selected = 'style_division'),
                               
                  checkboxInput('pb_stack', 'Expand items', value = FALSE)
                ),
                box(width = 4, selectizeGroupUI(
                  id = 'pb-usage-filters',
                  params = list(
                    division = list(inputId = 'division', title = 'Division'),
                    pi_name = list(inputId = 'pi_name', title = 'PI name')
                    )
                  )
                ),
                box(width = 5, 
                     #valueBoxOutput('output'), 
                     fluidRow(
                       valueBoxOutput('pb_runhours', width = 6), 
                       valueBoxOutput('pb_usage', width = 6)
                     )
                     #htmlOutput('selected_dates')
                 )
               ),
               fluidRow(
                 box(width = 12,
                   timevisOutput('pb_usage_timevis')
                     ),
                 box(width = 12, 
                     dataTableOutput('pb_usage_datatable')
                     )
               )
              ),
      tabPanel('PacBio output',
               fluidRow(
                 box(
                   width = 3,
                   dateRangeInput(
                     'pb_dates2',
                     'Select interval for PacBio output calculation',
                     min = Sys.Date() - years(5),
                     max = Sys.Date() + years(1),
                     separator = '--',
                     start = Sys.Date() - months(12)
                   ),
                   selectizeGroupUI(
                     id = 'pb-output-filters',
                     #label = 'Filter data by Division and PI',
                     params = list(
                       division = list(inputId = 'division', title = 'Division'),
                       pi_name = list(inputId = 'pi_name', title = 'PI name')
                     )
                   ),
                   selectizeInput(
                     'pb_time_units',
                     'Select time unit to aggregate data',
                     choices = c('week', 'month', 'year'),
                     selected = 'month'
                   ),
                   selectizeInput(
                     'pb_output_units',
                     'Select output unit',
                     choices = list(
                       'Flowcells' = 'fc',
                       'Bases' = 'sum_ccs_bases',
                       'Reads' = 'sum_ccs_reads'
                     ),
                     selected = 'fc'
                   ),
                   selectizeInput(
                     'pb_output_groupby',
                     'Group data by',
                     choices = c('Instrument' = 'instrumentType', 'Division' = 'division', 'PI' = 'pi_name'),
                     selected = 'instrumentType'
                   ),
                   checkboxInput('pb_output_type', 'Cumulative output', value = FALSE)
                 ),
                 box(
                   width = 3,
                   valueBoxOutput('pb_flowcells', width = 12),
                   valueBoxOutput('pb_output', width = 12)
                 ),
                 box(width = 6,
                     highchartOutput('pb_output_graph'))
               ),
               fluidRow(box(
                 width = 12,
                 dataTableOutput('pb_output_table')
               ))),
      tabPanel('PacBio status',
               box(width =12,
                 #column(width = 3, tags$p('machine1')),
                 uiOutput('pb_status')
               ))
      )
    
  )
)

# change in prod env
userauth <- F

if (userauth) {
ui <- secure_app(ui,theme = "simplex")
} else {
ui
}


