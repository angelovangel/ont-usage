#! /usr/bin/env bash

# one-time step: fill the per-file cache from an existing data/counts.csv, so nothing is recounted
# takes size and mtime from the files as they are now, and never overwrites a cache entry
# check the result with: bin/update-counts.sh, which recreates data/counts.csv from the cache

version=$(bin/count-seq-summary.sh --version)
seeded=0; existing=0; nofile=0

while IFS= read -r line; do
  [ -z "$line" ] && continue
  path="${line%%,*}"
  key="${path#data/}"
  cache="data/cache/counts/$key.tsv"
  if [ -e "$cache" ]; then existing=$((existing + 1)); continue; fi
  if ! stamp=$(stat -c $'%s\t%Y' "$path" 2>/dev/null); then
    echo "no file for row: $path"
    nofile=$((nofile + 1))
    continue
  fi
  values=$(tr ',' '\t' <<<"${line#*,}")
  mkdir -p "$(dirname "$cache")"
  printf '%s\t%s\t%s\t%s\n' "$key" "$stamp" "$version" "$values" > "$cache"
  seeded=$((seeded + 1))
done < data/counts.csv

echo "seeded $seeded, already cached $existing, no file $nofile"
