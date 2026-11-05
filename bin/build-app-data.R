#!/usr/bin/env Rscript

# prepare data for app from the final_summary rows and the count cache
# writes to data/app/, only if the checks below pass
# usage: build-app-data.R [--force], force publishes despite failed checks, e.g. after removing runs on purpose
library(vroom)
library(dplyr)
library(ivs)
library(stringr)

source('R/global.R')

app_dir <- 'data/app'
pis_file <- 'config/pis.tsv'
overrides_file <- 'config/run_overrides.tsv'
pb_file <- 'data/raw/pb_dump.csv'

dir.create(app_dir, showWarnings = F, recursive = T)
pis <- vroom(pis_file, show_col_types = F)

# PacBio dump data, same as before

df_pb <-
  vroom(pb_file, show_col_types = F) %>%
  mutate(px = str_extract(string = str_c(run_name, coll_name, sep = "_"), pattern = '(?<=_)P[0-9]{1,2}')) %>%
  # fix non-conformant runs
  mutate(
    px = case_when(
      str_detect(run_name, '_Peng') ~ 'P8',
      str_detect(run_name, '_Sara') ~ 'P1',
      str_detect(run_name, '_Damien') ~ 'P27',
      str_detect(run_name, 'SAT_') ~ 'P0',
      str_detect(run_name, 'Training') ~ 'P0',
      TRUE ~ px)
    ) %>%
  left_join(pis, by = c('px' = 'pi')) %>%
  # these are factors to enable color assignment
  mutate(division = factor(division), pi_portal = factor(pi_portal))

divisions <- setNames(
    levels(df_pb$division),
    my_levels_color(length(levels(df_pb$division)))
    ) %>%
  stack()

df_pb <-
  df_pb %>%
  left_join(divisions, by = c('division' = 'values')) %>%
  mutate(style_division = paste0('background-color: ', ind, ';'),
         style_num_cells = paste0('background-color: ', my_temp_color(totalCells, 0, 8), ';'))

# fix 1 wrong run enddate
df_pb[df_pb$run_uniqueId == '12d845e8-54f1-40d0-9580-f4a173023ae3',]$run_completedAt <- as_datetime("2019-05-06 08:36:59 UTC")

# ONT data
runs <- vroom(
  'data/cache/runs.tsv', delim = '\t', quote = '', show_col_types = F,
  col_types = cols(
    .default = col_guess(), run_id = col_character(), content = col_character(),
    protocol_group_id = col_character(), sample_id = col_character()
    )
  )

counts <- vroom(
  'data/cache/counts.tsv', delim = '\t', quote = '',
  col_names = c('key', 'size', 'mtime', 'version', 'bases_pass', 'bases_fail',
                'reads_pass', 'reads_fail', 'nx_pass', 'nx_fail', 'mean_qscore'),
  col_types = cols(key = col_character(), .default = col_double())
  ) %>%
  dplyr::select(-size, -mtime, -version) %>%
  # mean_qscore is NA only for runs without basecalling, the file has nothing to count
  mutate(count_status = if_else(is.na(mean_qscore), 'not_basecalled', 'counted')) %>%
  # otherwise a row means the file was counted, blank values are zero (no reads, or no pass reads)
  mutate(across(
    c(bases_pass, bases_fail, reads_pass, reads_fail, nx_pass, nx_fail),
    ~ if_else(count_status == 'counted', coalesce(.x, 0), NA_real_)
    ))

# one row per run. The same run can be in two directories (e.g. encrypted_ copy), keep the one with the most
# pass bases, the others are partial copies. Runs without counts come last
runs_counts <- runs %>%
  left_join(counts, by = c('seq_summary_key' = 'key')) %>%
  arrange(desc(bases_pass), final_summary)

copies <- runs_counts %>% group_by(run_id) %>% dplyr::filter(n() > 1) %>% ungroup()
differ <- copies %>% group_by(run_id) %>% dplyr::filter(n_distinct(bases_pass, na.rm = T) > 1) %>% ungroup()
message(n_distinct(copies$run_id), ' runs in more than one directory, ', n_distinct(differ$run_id), ' of these with different counts')
if (nrow(differ) > 0) { message(paste('  ', differ$final_summary, differ$bases_pass, collapse = '\n')) }

runs_counts <- runs_counts %>% distinct(run_id, .keep_all = T)

# PI comes from protocol_group_id, then the directory names, run_overrides.tsv is for runs where it is in neither
if (file.exists(overrides_file)) {
  overrides <- vroom(overrides_file, delim = '\t', col_types = cols(.default = col_character())) %>%
    dplyr::select(run_id, pi_override = pi)
  unknown <- setdiff(overrides$run_id, runs_counts$run_id)
  if (length(unknown) > 0) { message('overrides for unknown runs: ', str_flatten(unknown, collapse = ', ')) }
} else {
  overrides <- tibble(run_id = character(), pi_override = character())
}

# POUO is a typo for P0U0. Only this spelling, POU1 can't be corrected without knowing the project
find_px <- function(x) {
  str_extract(
    string = str_replace_all(x, "(?<![A-Za-z0-9])POUO", "P0U0"),
    pattern = "(?<![A-Za-z0-9])P[0-9]{1,2}[PU][0-9]{1,2}"
    )
}

df <- runs_counts %>%
  left_join(overrides, by = 'run_id') %>%
  mutate(
    type = 'range',
    id = run_id,
    flowcell_id = str_c(content, "_", str_extract(string = run_id, pattern = "^[a-z0-9]{8}(?=-)")), # gives e.g. FAR33076_074e9094
    px = coalesce(
      find_px(protocol_group_id),
      find_px(final_summary),
      # plasmid runs without a code are P0
      if_else(str_detect(final_summary, '(?i)plas?mid'), 'P0U0', NA_character_)
      ),
    pi = coalesce(pi_override, str_extract(string = px, pattern = "P[0-9]{1,2}")), # PI only to join to pis
    ratio = if_else(bases_pass > 0, bases_fail/bases_pass, NA_real_),
    style_failed = paste0('background-color: ', my_temp_color(ratio, 1, 0), ';'),
    style_qscore = paste0('background-color: ', my_temp_color(mean_qscore, 0, 22), ';'),
    style_nx = paste0('background-color: ', my_temp_color(nx_pass, 0, 30000), ';')
    ) %>%
  dplyr::select(-pi_override) %>%
  left_join(pis, by = 'pi') %>%
  mutate(title = case_when(
    count_status %in% 'not_basecalled' ~ 'not basecalled',
    is.na(bases_pass) ~ 'no counts',
    bases_pass == 0 ~ paste0('no pass reads | ', siformat(coalesce(bases_fail, 0)), ' failed bases'),
    TRUE ~ paste0(
      siformat(coalesce(bases_pass, 0)), ' bases | ',
      siformat(coalesce(reads_pass, 0)), ' reads | ',
      round(ratio * 100, 0), '% failed | ',
      'qscore ', mean_qscore, ' | N50 ', nx_pass
      )
    )) %>%
  arrange(start)

# report what the app can't show properly
report <- function(x, what) {
  message(nrow(x), ' runs ', what)
  if (nrow(x) > 0) { message(paste('  ', x$final_summary, x$protocol_group_id, collapse = '\n')) }
}
report(df %>% dplyr::filter(is.na(pi)), 'without PI, add to run_overrides.tsv')
report(df %>% dplyr::filter(!is.na(pi), is.na(pi_name)), 'with a PI not in pis.tsv')
report(df %>% dplyr::filter(count_status %in% 'not_basecalled'), 'not basecalled')
report(df %>% dplyr::filter(is.na(count_status)), 'without counts')

# replace in one step, a running app never reads a half-written file
publish <- function(x, name) {
  tmp <- file.path(app_dir, paste0('.', name, '.tmp'))
  write.csv(x, file = tmp, row.names = F)
  invisible(file.rename(tmp, file.path(app_dir, name)))
}

# checks before publishing, runs only accumulate so the numbers should not go down
max_drop <- 0.01 # total pass bases
max_no_counts <- 0.1 # share of runs

read_previous <- function(name) {
  f <- file.path(app_dir, name)
  if (file.exists(f)) { vroom(f, show_col_types = F) }
}

check_ont <- function(new, prev) {
  need <- c('id', 'group', 'start', 'end', 'content', 'flowcell_id', 'sample_id', 'pi_name', 'division', 'title', 
            'bases_pass', 'reads_pass', 'mean_qscore', 'nx_pass', 'style_failed', 'style_qscore', 'style_nx')
  missing_runs <- new$final_summary[is.na(new$start) | is.na(new$end)]
  no_counts <- mean(is.na(new$count_status))
  c(
    if (nrow(new) == 0) 'no ONT runs',
    if (!all(need %in% names(new))) paste('missing columns:', str_flatten(setdiff(need, names(new)), collapse = ', ')),
    if (anyDuplicated(new$id) > 0) 'duplicate run ids',
    if (length(missing_runs) > 0) paste('runs without start or end:', str_flatten(missing_runs, collapse = ', ')),
    if (no_counts > max_no_counts) paste0(round(no_counts * 100), '% of runs without counts'),
    if (!is.null(prev) && nrow(new) < nrow(prev)) paste('ONT runs went down:', nrow(prev), 'to', nrow(new)),
    if (!is.null(prev) && sum(new$bases_pass, na.rm = T) < (1 - max_drop) * sum(prev$bases_pass, na.rm = T)) {
      paste('total pass bases went down:', sum(prev$bases_pass, na.rm = T), 'to', sum(new$bases_pass, na.rm = T))
    }
  )
}

check_pb <- function(new, prev) {
  c(
    if (!all(c('run_uniqueId', 'division', 'pi_name') %in% names(new))) 'missing PacBio columns',
    if (!is.null(prev) && nrow(new) < nrow(prev)) paste('PacBio rows went down:', nrow(prev), 'to', nrow(new))
  )
}

problems <- c(
  check_ont(df, read_previous('df.csv')), 
  check_pb(df_pb, read_previous('pb_dump_processed.csv'))
)
if (length(problems) > 0) {
  message('checks failed:\n', paste(' ', problems, collapse = '\n'))
  if (!'--force' %in% commandArgs(trailingOnly = T)) { quit(status = 1) }
  message('publishing anyway, --force')
}

publish(df_pb, 'pb_dump_processed.csv')
# the app shows this date as PacBio data update, keep it at the time of the dump
Sys.setFileTime(file.path(app_dir, 'pb_dump_processed.csv'), file.mtime(pb_file))
publish(df, 'df.csv')
message('data/app/df.csv has ', nrow(df), ' runs')
