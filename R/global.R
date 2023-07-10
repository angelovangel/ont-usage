library(optparse)
library(vroom)
library(purrr)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(timevis)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ivs) # https://cran.r-project.org/web/packages/ivs/vignettes/ivs.html
library(vctrs)
library(highcharter)
#library(digest)
library(scales)

siformat <- function(x) {system2('bin/siformat.sh', args = x, stdout = T)}


# data loaded once for all sessions
df <- vroom('data/df.csv') %>% 
  mutate(group = factor(group))

groups_df <- data.frame(
  id = c('grid', 'prom'),
  content = c('GridION', 'PromethION')
)


# heatmap for some values
# pass x as argument, return hex
my_temp_color <- function(x, xfrom, xto) {
  rainbow <- c("#2ECC71", 
               "#DAF7A6", 
               "#FFC300",
               "#FF5733")
  # set scale 
  # color ramp used for temp gradient
  myramp <- scales::colour_ramp(rev(rainbow), na.color = "#FF5733")
  scaled_x <- scales::rescale( x, from = c(xfrom, xto), to = c(0,1) )
  myramp(scaled_x)
}

# merge overlapping intervals, by group
merge_overlaps <- function(dataframe, start_time, end_time, grouping) {
  dataframe %>%
    ungroup %>%
    mutate(rng = iv({{start_time}}, {{end_time}})) %>%
    reframe(rng = iv_groups(rng), .by = {{grouping}}) %>%
    #rowwise() %>%
    mutate(
      content = paste0({{grouping}}, '-merged'),
      start = vctrs::field(rng, 1),
      end = vctrs::field(rng, 2),
      dur = end - start
      ) 
}

