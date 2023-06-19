#!/usr/bin/env Rscript

# prepare data for app


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



df2 <- vroom(count_files, col_names = c('file', 'bases_pass', 'bases_fail', 'reads_pass', 'reads_fail')) %>%
  dplyr::distinct() %>%
  # to get colors based on bases, before siformat
  mutate(ratio = bases_fail/bases_pass, style = paste0('background-color: ', my_temp_color(ratio, 0, 1), ';')) %>%
  mutate(flowcell = str_extract(string = file, pattern = '(?<=summary_)[A-Z]+[0-9]+'))

df <- df1 %>% 
  left_join(df2, by = c('seq_summary_file' = 'file')) %>%
  mutate(title = paste0(
    siformat(bases_pass), ' bases | ', 
    siformat(reads_pass), ' reads | ',
    round(ratio * 100, 0), '% failed'
  )
  ) %>%
  arrange(start)

dfmerged <- df %>%
  merge_overlaps(start, end, group)

write.csv(df, file = 'data/df.csv', row.names = F)
saveRDS(dfmerged, file = 'data/dfmerged.rds')

