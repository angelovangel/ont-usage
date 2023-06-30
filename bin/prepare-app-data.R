#!/usr/bin/env Rscript

# prepare data for app
library(vroom)
library(dplyr)
library(ivs)
library(stringr)

source('R/global.R')

siformat <- function(x) {system2('bin/siformat.sh', args = x, stdout = T)}

processed_files <- list.files('data', pattern = 'grid.csv|prom.csv', full.names = T, recursive = F)
count_files <- list.files('data', pattern = 'counts.csv', full.names = T, recursive = F)

df1 <- vroom(processed_files) %>% 
  dplyr::distinct() %>%
  mutate(type = 'range') %>%
  rowwise() %>% 
  mutate(id = run_id)
#mutate(id = digest(rnorm(1)))
# create id



df2 <- vroom(count_files, 
             col_names = c('file', 'bases_pass', 'bases_fail', 
                           'reads_pass', 'reads_fail', 
                           'nx_pass', 'nx_fail', 'mean_qscore')
             ) %>%
  mutate(file = basename(file)) %>%
  dplyr::distinct() %>%
  # use basename of file to match the final_summary seq_summary_file
  mutate(ratio = bases_fail/bases_pass, 
         style_failed = paste0('background-color: ', my_temp_color(ratio, 0, 1), ';'),
         style_qscore = paste0('background-color: ', my_temp_color(mean_qscore, 20, 0), ';')
         ) %>%
  mutate(flowcell = str_extract(string = file, pattern = '(?<=summary_)[A-Z]+[0-9]+'))

df <- df1 %>% 
  left_join(df2, by = c('seq_summary_file' = 'file')) %>%
  mutate(title = paste0(
    siformat(bases_pass), ' bases | ', 
    siformat(reads_pass), ' reads | ',
    round(ratio * 100, 0), '% failed | ',
    'qscore ',mean_qscore
  )
  ) %>%
  arrange(start)

dfmerged <- df %>%
  merge_overlaps(start, end, group)

write.csv(df, file = 'data/df.csv', row.names = F)
saveRDS(dfmerged, file = 'data/dfmerged.rds')

