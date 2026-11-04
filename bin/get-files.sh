#! /usr/bin/env bash

# only runs dirs starting with 20* in /data/
# run as a cronjob, deleted files in source don't get deleted in dest
# rsync -avrm --progress --include='20*/' --include='final_summary*' --exclude='*' prom@promethion.bcl.kaust.edu.sa:/data/ data/prom/
# rsync -avrm --progress --include='20*/' --include='final_summary*' --exclude='*' grid@gridion.bcl.kaust.edu.sa:/data/ data/grid/

# only one run at a time, overlapping runs append the same files to counts.csv
exec 9> data/.get-files.lock
flock -n 9 || { echo "$(date +'%Y-%m-%d %H:%M:%S') === could not get lock, another get-files.sh is running? exiting"; exit 0; }

echo "$(date +'%Y-%m-%d %H:%M:%S') === starting get-files.sh, running rsync ..."

# top-level test/tmp dirs to skip (must come before the includes)
EXCLUDE_DIRS=(
--exclude='/scratch/'
--exclude='/temp-yingzi/'
--exclude='/Test_run/'
--exclude='/testdir/'
--exclude='/TestRUN_3062026/'
--exclude='/tgspipe_testcases/'
--exclude='/tmp/'
)

rsync -avm --progress \
"${EXCLUDE_DIRS[@]}" \
--include='*/' \
--include='*final_summary*' \
--include='*sequencing_summary*' \
--exclude='*' \
nanopore@mover.ibex.kaust.edu.sa:/biocorelab/Genomics/RawData/PromethION/ data/prom/

# separately for encrypted
rsync -avm --progress \
"${EXCLUDE_DIRS[@]}" \
--include='*/' \
--include='*final_summary*' \
--include='*sequencing_summary*' \
--exclude='*' \
nanopore@mover.ibex.kaust.edu.sa:/encrypted0/biocorelab/Genomics/RawData/PromethION/ data/prom/


rsync -avm --progress \
"${EXCLUDE_DIRS[@]}" \
--include='*/' \
--include='*final_summary*' \
--include='*sequencing_summary*' \
--exclude='*' \
nanopore@mover.ibex.kaust.edu.sa:/biocorelab/Genomics/RawData/GridION/ data/grid/

# separately for encrypted
rsync -avm --progress \
"${EXCLUDE_DIRS[@]}" \
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
grep -vFf data/seqsum-fromcounts.files data/seqsum.files | parallel --joblog data/count.joblog bin/count-seq-summary.sh 0.5 >> data/counts.csv

# failed jobs add no row to counts.csv and are retried on the next run
echo "failed count jobs (exit code or signal, see data/count.joblog): $(awk -F'\t' 'NR>1 && ($7!=0 || $8!=0)' data/count.joblog | wc -l)"

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
