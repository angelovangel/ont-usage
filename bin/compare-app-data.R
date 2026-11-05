#!/usr/bin/env Rscript

# compare the old data/df.csv with the new data/app/df.csv, by run id
# prints a summary and writes every difference to data/app-compare.tsv
library(vroom)
library(dplyr)
library(tidyr)

read_all <- function(f) {vroom(f, col_types = cols(.default = col_character()), show_col_types = F)}

old <- read_all('data/df.csv') %>% rename(flowcell_id = flowcell_id.x)
new <- read_all('data/app/df.csv')

cols_compare <- c('group', 'start', 'end', 'seqend', 'content', 'flowcell_id', 'sample_id', 'protocol_group_id',
                  'protocol', 'seq_summary_file', 'px', 'pi', 'pi_name', 'division', 'bases_pass', 'bases_fail',
                  'reads_pass', 'reads_fail', 'nx_pass', 'nx_fail', 'mean_qscore')

cat('rows: old', nrow(old), 'new', nrow(new), '\n')
cat('duplicate ids: old', sum(duplicated(old$id)), 'new', sum(duplicated(new$id)), '\n')

only_old <- old %>% anti_join(new, by = 'id')
only_new <- new %>% anti_join(old, by = 'id')
cat('only in old:', nrow(only_old), ' only in new:', nrow(only_new), '\n')

long <- function(x, value) {
  x %>%
    distinct(id, .keep_all = T) %>%
    dplyr::select(id, all_of(cols_compare)) %>%
    pivot_longer(-id, names_to = 'column', values_to = value)
}

diffs <- inner_join(long(old, 'old'), long(new, 'new'), by = c('id', 'column')) %>%
  # both NA is not a difference, one NA is
  dplyr::filter(coalesce(old != new, is.na(old) != is.na(new)))

cat('\ndifferences per column, for runs in both:\n')
diffs %>% count(column, sort = T) %>% as.data.frame() %>% print(row.names = F)

totals <- function(x) {c(bases_pass = sum(as.numeric(x$bases_pass), na.rm = T), reads_pass = sum(as.numeric(x$reads_pass), na.rm = T))}
cat('\ntotals old:', format(totals(old), big.mark = ','), '\n')
cat('totals new:', format(totals(new), big.mark = ','), '\n')

bind_rows(
  diffs,
  only_old %>% transmute(id, column = 'only_in_old', old = flowcell_id, new = NA_character_),
  only_new %>% transmute(id, column = 'only_in_new', old = NA_character_, new = flowcell_id)
) %>%
  arrange(column, id) %>%
  vroom_write('data/app-compare.tsv', delim = '\t')
cat('\nall differences are in data/app-compare.tsv\n')
