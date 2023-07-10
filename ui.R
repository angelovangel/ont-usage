
ui <- dashboardPage(
  skin = 'green',
  header = dashboardHeader(title = 'TGS machine usage (BCL)', titleWidth = 350,
                           dropdownMenu(type = 'notifications', 
                                        notificationItem(
                                          text = paste0('ONT data update: ', file.mtime('data/df.csv')))
                           )
  ),
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
                                    start = Sys.Date() - months(12)),
                     
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
      tabPanel('PacBio usage'),
      tabPanel('PacBio output')
    )
  )
)
