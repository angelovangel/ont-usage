#!/usr/bin/env Rscript
# prepare a csv dump of a SMRTLink v12.0 database
# SMRT_BASE, SMRT_USER and SMRT_PASS come from the environment or .Renviron, not from the command line

if (Sys.getenv('SMRT_BASE') == '') {
  message('SMRT env variables not set, skipping PacBio dump')
  quit(status = 0)
}

library(smrtlinker) # use .Renviron to install from private repo and provide use/pass

pb_dump <- smrtlinker:::prep_dump(baseurl = Sys.getenv('SMRT_BASE'), user = Sys.getenv('SMRT_USER'), pass = Sys.getenv('SMRT_PASS'))
write.csv(pb_dump, file = 'data/raw/pb_dump.csv', row.names = F)
