#! /usr/bin/env bash

# count one sequencing summary file and write the result to the per-file cache
# usage
# count-one.sh prom/rundir/sequencing_summary_x.txt
#
# the key is relative to the prom/ and grid/ roots, so it survives moving these
# cache row, tab separated (a failed count writes nothing and is retried on the next run):
# key size mtime version bases_pass bases_fail reads_pass reads_fail nx_pass nx_fail mean_qscore

key="$1"
src="data/raw/$key"
cache="data/cache/counts/$key.tsv"

# stat before counting, if the file changes meanwhile the next run sees it as stale
stamp=$(stat -c $'%s\t%Y' "$src") || exit 1
version=$(bin/count-seq-summary.sh --version)
row=$(bin/count-seq-summary.sh 0.5 "$src")
status=$?

if [ "$status" -eq 3 ]; then
  # run without basecalling, cached so it is not read again. mean_qscore NA marks these rows
  values=$'\t\t\t\t\t\tNA'
elif [ "$status" -ne 0 ]; then
  exit 1
else
  # the last 7 fields are the values, the path comes first
  values=$(gawk -F, -v OFS='\t' '{print $(NF-6), $(NF-5), $(NF-4), $(NF-3), $(NF-2), $(NF-1), $NF}' <<<"$row")
fi

mkdir -p "$(dirname "$cache")"
tmp="$cache.tmp.$$"
printf '%s\t%s\t%s\t%s\n' "$key" "$stamp" "$version" "$values" > "$tmp" && mv "$tmp" "$cache"
