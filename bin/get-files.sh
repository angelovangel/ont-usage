#! /usr/bin/env bash

# only runs dirs starting with 20* in /data/
# run as a cronjob, deleted files in source don't get deleted in dest
# rsync -avrm --progress --include='20*/' --include='final_summary*' --exclude='*' prom@promethion.bcl.kaust.edu.sa:/data/ data/prom/
# rsync -avrm --progress --include='20*/' --include='final_summary*' --exclude='*' grid@gridion.bcl.kaust.edu.sa:/data/ data/grid/

echo "$(date +'%Y-%m-%d %H:%M:%S') === starting get-files.sh, running rsync ..."

rsync -avm --progress \
--include='*/' \
--include='*final_summary*' \
--include='*sequencing_summary*' \
--exclude='*' \
nanopore@mover.ibex.kaust.edu.sa:/biocorelab/Genomics/RawData/PromethION/ data/prom/

# separately for encrypted
rsync -avm --progress \
--include='*/' \
--include='*final_summary*' \
--include='*sequencing_summary*' \
--exclude='*' \
nanopore@mover.ibex.kaust.edu.sa:/encrypted0/biocorelab/Genomics/RawData/PromethION/ data/prom/


rsync -avm --progress \
--include='*/' \
--include='*final_summary*' \
--include='*sequencing_summary*' \
--exclude='*' \
nanopore@mover.ibex.kaust.edu.sa:/biocorelab/Genomics/RawData/GridION/ data/grid/

# separately for encrypted
rsync -avm --progress \
--include='*/' \
--include='*final_summary*' \
--include='*sequencing_summary*' \
--exclude='*' \
nanopore@mover.ibex.kaust.edu.sa:/encrypted0/biocorelab/Genomics/RawData/GridION/ data/grid/

echo "$(date +'%Y-%m-%d %H:%M:%S') === rsync done, will update data/counts.csv ..."
# find all sequencing summary files:
find data -type f -name '*sequencing_summary*' > data/seqsum.files

# get the sequencing summary files that are already in counts.csv
cat data/counts.csv | cut -f1 -d, > data/seqsum-fromcounts.files

# get files in seqsum.files NOT present in counts and run script on them
newfiles=$(grep -vFf data/seqsum-fromcounts.files data/seqsum.files | wc -l)
echo "found $newfiles new sequencing summary files"
grep -vFf data/seqsum-fromcounts.files data/seqsum.files | parallel bin/count-seq-summary.sh 0.5 >> data/counts.csv

# dump PacBio data
echo "$(date +'%Y-%m-%d %H:%M:%S') === running pb-dump.R..."
source .Renviron
[ ! -z $SMRT_BASE ] && bin/pb-dump.R $SMRT_BASE $SMRT_USER $SMRT_PASS  || echo 'SMRT env variables not set, skipping PacBio dump'


# process data to make a csv for app and sharing

echo "$(date +'%Y-%m-%d %H:%M:%S') === counts done, running process-files.R ..."
bin/process-files.R -p prom -r data/prom.csv data/prom
bin/process-files.R -p grid -r data/grid.csv data/grid

echo "$(date +'%Y-%m-%d %H:%M:%S') === process-files.R done, running prepare-app-data.R ..."
bin/prepare-app-data.R

echo "$(date +'%Y-%m-%d %H:%M:%S') === Done"
