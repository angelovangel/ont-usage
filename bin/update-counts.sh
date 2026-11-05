#! /usr/bin/env bash

# bring the per-file count cache up to date, then generate the count tables from it
# run bin/parse-final-summaries.sh first, it makes the list of files
# a file is counted again if its size, mtime or the count script version changed
# COUNT_JOBS sets the parallel jobs. Default 1, the disk gave 250 MB/s to one job and 60-90 MB/s in total to 16

# without this an unseeded cache would recount every file
if [ -s data/counts.csv ] && [ ! -d data/cache/counts ]; then
  echo "ERROR: data/counts.csv exists but the cache is empty, run bin/seed-count-cache.sh first"
  exit 1
fi

version=$(bin/count-seq-summary.sh --version)
stale=$(mktemp data/.stale.XXXXXX)
trap 'rm -f "$stale"' EXIT

# only summaries referred to by a final_summary, which is written when the run has ended
awk -F'\t' 'NR>1 && $12!="" {print $12}' data/cache/runs.tsv | sort -u |
while IFS= read -r key; do
  stamp=$(stat -c $'%s\t%Y' "data/raw/$key" 2>/dev/null) || continue

  cached=$(cut -f2-4 "data/cache/counts/$key.tsv" 2>/dev/null)
  [ "$cached" = "$stamp"$'\t'"$version" ] || printf '%s\n' "$key"
done > "$stale"

echo "found $(wc -l < "$stale") sequencing summary files to count"
if [ -s "$stale" ]; then
  parallel -j "${COUNT_JOBS:-1}" --joblog data/count.joblog bin/count-one.sh < "$stale"
  # failed jobs add no row to the cache and are retried on the next run
  echo "failed count jobs (exit code or signal, see data/count.joblog): $(awk -F'\t' 'NR>1 && ($7!=0 || $8!=0)' data/count.joblog | wc -l)"
fi

tsv=$(mktemp data/.counts-tsv.XXXXXX)
csv=$(mktemp data/.counts-csv.XXXXXX)
trap 'rm -f "$stale" "$tsv" "$csv"' EXIT

find data/cache/counts -type f -name '*.tsv' -print0 | sort -z | xargs -0 -r cat > "$tsv"
awk -F'\t' -v OFS=',' '{print "data/"$1, $5, $6, $7, $8, $9, $10, $11}' "$tsv" > "$csv"

# the cache only grows, fewer rows than before means it was not seeded or something went wrong
new=$(wc -l < "$csv")
old=0
[ -f data/counts.csv ] && old=$(wc -l < data/counts.csv)
if [ "$new" -lt "$old" ]; then
  echo "ERROR: cache has $new rows, data/counts.csv has $old. Not replacing it, run bin/seed-count-cache.sh?"
  exit 1
fi

chmod 664 "$tsv" "$csv"
mv "$tsv" data/cache/counts.tsv
mv "$csv" data/counts.csv
echo "data/counts.csv has $new rows"
