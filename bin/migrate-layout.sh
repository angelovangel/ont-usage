#! /usr/bin/env bash

# one-time step: move the input files to data/raw/ and config/, nothing is copied or deleted
# stops before moving anything if a target exists or a file is missing, safe to run again

exec 9> data/.get-files.lock
flock -n 9 || { echo "ERROR: get-files.sh is running, try later"; exit 1; }

[ -d data/cache/counts ] || { echo "ERROR: no count cache, run bin/seed-count-cache.sh first"; exit 1; }

moves=(
  data/prom:data/raw/prom
  data/grid:data/raw/grid
  data/pb_dump.csv:data/raw/pb_dump.csv
  data/pis.tsv:config/pis.tsv
  data/credentials.rds:config/credentials.rds
  data/run_overrides.tsv:config/run_overrides.tsv
)
# no overrides file is fine, the others are needed
optional=data/run_overrides.tsv

todo=()
for m in "${moves[@]}"; do
  src="${m%%:*}"; dest="${m##*:}"
  if [ -e "$src" ] && [ -e "$dest" ]; then
    echo "ERROR: both $src and $dest exist, nothing moved"; exit 1
  elif [ -e "$src" ]; then
    todo+=("$m")
  elif [ ! -e "$dest" ] && [ "$src" != "$optional" ]; then
    echo "ERROR: $src not found, nothing moved"; exit 1
  fi
done

mkdir -p data/raw config
for m in ${todo[@]+"${todo[@]}"}; do
  src="${m%%:*}"; dest="${m##*:}"
  mv -n "$src" "$dest" && echo "moved $src to $dest"
done
echo "done"
