#!/usr/bin/env Rscript

# process final_summary files and generate a data.frame
# output csv to stdout or file

PKGs <- c("optparse", "vroom", "stringr", "dplyr", 'purrr')
loadPKGs <- function(x) { suppressPackageStartupMessages(library(x, character.only = TRUE)) }

invisible(lapply(PKGs, loadPKGs))

### getting-setting arguments with optparse

option_list <- list(
  make_option(c("-r", "--result"), 
              default = "", 
              type = "character", 
              help = "name of output csv file, leave empty to output to stdout [default=%default]"),
  make_option(c("-p", "--platform"), 
              default = "prom", 
              type = "character", 
              help = "platform, will be added to output dataframe")
)

opt_parser <- OptionParser(option_list=option_list, 
                           description = "Find final_summary_xx.txt files and create a dataframe with all data", 
                           usage = "usage: %prog [options] path",
                           epilogue = "2023 | KAUST-BCL | aangeloo@gmail.com")

arguments <- parse_args(opt_parser, positional_arguments = 1)
opt <- arguments$options
mypath <- arguments$args

myfiles <- list.files(mypath, pattern = 'final_summary*', recursive = T, full.names = T)
mydirs <- dirname(myfiles)
myrundirs <- basename(mydirs) 

df <- vroom_lines(myfiles)

getlines <- function(x, y) {
  x[str_detect(x, y)] %>% str_remove(y)
}

# use column names for timevis
mylist = list(
  start = 'started=',
  end = 'processing_stopped=',
  id = 'protocol_run_id=',
  seqend = 'acquisition_stopped=',
  content = 'flow_cell_id=',
  protocol_group_id = 'protocol_group_id=',
  title = 'sample_id=',
  protocol = 'protocol=',
  seq_summary_file = 'sequencing_summary_file='
)

result <- map_dfc(mylist, getlines, x= df)
result$group <- opt$p

write.csv(result, file = opt$r, row.names = F)

