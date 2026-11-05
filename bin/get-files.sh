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
nanopore@mover.ibex.kaust.edu.sa:/biocorelab/Genomics/RawData/PromethION/ data/raw/prom/

# separately for encrypted
rsync -avm --progress \
"${EXCLUDE_DIRS[@]}" \
--include='*/' \
--include='*final_summary*' \
--include='*sequencing_summary*' \
--exclude='*' \
nanopore@mover.ibex.kaust.edu.sa:/encrypted0/biocorelab/Genomics/RawData/PromethION/ data/raw/prom/


rsync -avm --progress \
"${EXCLUDE_DIRS[@]}" \
--include='*/' \
--include='*final_summary*' \
--include='*sequencing_summary*' \
--exclude='*' \
nanopore@mover.ibex.kaust.edu.sa:/biocorelab/Genomics/RawData/GridION/ data/raw/grid/

# separately for encrypted
rsync -avm --progress \
"${EXCLUDE_DIRS[@]}" \
--include='*/' \
--include='*final_summary*' \
--include='*sequencing_summary*' \
--exclude='*' \
nanopore@mover.ibex.kaust.edu.sa:/encrypted0/biocorelab/Genomics/RawData/GridION/ data/raw/grid/

echo "$(date +'%Y-%m-%d %H:%M:%S') === rsync done, will update data/counts.csv ..."
bin/parse-final-summaries.sh || { echo "$(date +'%Y-%m-%d %H:%M:%S') === parse-final-summaries.sh failed, exiting"; exit 1; }
bin/update-counts.sh || { echo "$(date +'%Y-%m-%d %H:%M:%S') === update-counts.sh failed, exiting"; exit 1; }

# dump PacBio data. Not fatal, the app keeps the last dump and shows its date
echo "$(date +'%Y-%m-%d %H:%M:%S') === running pb-dump.R..."
bin/pb-dump.R || echo "$(date +'%Y-%m-%d %H:%M:%S') === pb-dump.R failed, keeping data/raw/pb_dump.csv"

# checks the new data first, if it fails data/app stays as it is
echo "$(date +'%Y-%m-%d %H:%M:%S') === running build-app-data.R ..."
bin/build-app-data.R || { echo "$(date +'%Y-%m-%d %H:%M:%S') === build-app-data.R failed, data/app not changed, exiting"; exit 1; }

echo "$(date +'%Y-%m-%d %H:%M:%S') === Done"
