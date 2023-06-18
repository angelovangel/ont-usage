#! /usr/bin/env bash

# only runs dirs starting with 20* in /data/
# run as a cronjob, deleted files in source don't get deleted in dest
# rsync -avrm --progress --include='20*/' --include='final_summary*' --exclude='*' prom@promethion.bcl.kaust.edu.sa:/data/ data/prom/
# rsync -avrm --progress --include='20*/' --include='final_summary*' --exclude='*' grid@gridion.bcl.kaust.edu.sa:/data/ data/grid/

rsync -avrm --progress \
--include='*2*/' \
--include='final_summary*' \
--include='sequencing_summary*' \
--exclude='*' \
nanopore@mover.ibex.kaust.edu.sa:/biocorelab/Genomics/RawData/PromethION/ data/prom/

# separately for encrypted
rsync -avrm --progress \
--include='*2*/' \
--include='final_summary*' \
--include='sequencing_summary*' \
--exclude='*' \
nanopore@mover.ibex.kaust.edu.sa:/encrypted0/biocorelab/Genomics/RawData/PromethION/ data/prom/


rsync -avrm --progress \
--include='*2*/' \
--include='final_summary*' \
--include='sequencing_summary*' \
--exclude='*' \
nanopore@mover.ibex.kaust.edu.sa:/biocorelab/Genomics/RawData/GridION/ data/grid/

# separately for encrypted
rsync -avrm --progress \
--include='*2*/' \
--include='final_summary*' \
--include='sequencing_summary*' \
--exclude='*' \
nanopore@mover.ibex.kaust.edu.sa:/encrypted0/biocorelab/Genomics/RawData/GridION/ data/grid/

echo 'rsync done, will update data/counts.csv ...'
#
# find data -type f -name 'sequencing*' > data/seqsum.files
# cat data/counts.csv | cut -f1 -d, > data/seqsum-fromcounts.files
# grep -xvFf data/seqsum-fromcounts.files data/seqsum.files > data/seqsum-newfiles.txt
# run counts on the new seq files
# grep -Ff data/seqsum-newfiles.txt data/seqsum.files | parallel bin/count_seq_summary.sh >> data/counts.csv
# process data to make a csv for app and sharing

echo 'counts done, running process-files.R ...'
bin/process-files.R -p prom -r data/prom.csv data/prom
bin/process-files.R -p grid -r data/grid.csv data/grid

echo 'Done!'
