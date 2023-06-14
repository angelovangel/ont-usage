#! /usr/bin/env bash

# only runs dirs starting with 20* in /data/
# run as a cronjob, deleted files in source don't get deleted in dest
# rsync -avrm --progress --include='20*/' --include='final_summary*' --exclude='*' prom@promethion.bcl.kaust.edu.sa:/data/ data/prom/
# rsync -avrm --progress --include='20*/' --include='final_summary*' --exclude='*' grid@gridion.bcl.kaust.edu.sa:/data/ data/grid/

rsync -avrm --progress --include='2*/' --include='final_summary*' --exclude='*' nanopore@ilogin.ibex.kaust.edu.sa:/biocorelab/RawData/PromethION/ data/prom/
rsync -avrm --progress --include='2*/' --include='final_summary*' --exclude='*' nanopore@ilogin.ibex.kaust.edu.sa:/biocorelab/RawData/GridION/ data/grid/

# process data to make a csv for app and sharing
bin/process-files.R -p prom -r data/prom.csv data/prom 
bin/process-files.R -p grid -r data/grid.csv data/grid