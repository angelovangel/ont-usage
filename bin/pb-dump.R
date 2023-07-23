#!/usr/bin/env Rscript
# prepare a csv dump of a SMRTLink v12.0 database

library(smrtlinker) # use .Renviron to install from private repo and provide use/pass

args <- commandArgs(trailingOnly = T)
  
pb_dump <- smrtlinker:::prep_dump(baseurl = args[1], user = args[2], pass = args[3])
write.csv(pb_dump, file = 'data/pb_dump.csv', row.names = F)