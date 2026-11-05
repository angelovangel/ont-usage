#! /usr/bin/env bash

# read the final_summary files and write data/cache/runs.tsv, one row per file
# test runs (CTC flow cells, sample_id no_sample) are left out here, so their summaries are never counted
# seq_summary_key is the cache key of the sequencing summary this run refers to

mkdir -p data/cache
tmp=$(mktemp data/.runs.XXXXXX)
trap 'rm -f "$tmp"' EXIT

# empty final_summary files are from test/tmp directories, ._ files are macOS metadata
mapfile -t files < <(find data/raw/prom data/raw/grid -type f -name '*final_summary*.txt' ! -name '._*' -size +0 | sort)
[ "${#files[@]}" -gt 0 ] || { echo "ERROR: no final_summary files found"; exit 1; }

gawk -v OFS='\t' '
BEGIN {
  print "group", "final_summary", "run_id", "start", "end", "seqend", "content",
        "protocol_group_id", "sample_id", "protocol", "seq_summary_file", "seq_summary_key"
}
BEGINFILE { delete f }
{
  sub(/\r$/, "")
  gsub(/\t/, " ")
  eq = index($0, "=")
  if (eq) f[substr($0, 1, eq - 1)] = substr($0, eq + 1)
}
ENDFILE {
  # data/raw/<group>/<relative dir>/<file>
  rel = FILENAME
  sub(/^data\/raw\//, "", rel)
  dir = rel
  sub(/\/[^\/]*$/, "", dir)
  group = rel
  sub(/\/.*$/, "", group)
  summary = f["sequencing_summary_file"]

  if (f["protocol_run_id"] == "") {
    print "no protocol_run_id, skipped: " FILENAME > "/dev/stderr"
    noid++
  } else if (f["flow_cell_id"] ~ /^CTC/ || f["sample_id"] == "no_sample") {
    excluded++
  } else {
    print group, rel, f["protocol_run_id"], f["started"], f["processing_stopped"], f["acquisition_stopped"],
          f["flow_cell_id"], f["protocol_group_id"], f["sample_id"], f["protocol"], summary,
          (summary == "" ? "" : dir "/" summary)
    kept++
  }
}
END {
  printf "final_summary files: %d runs, %d test runs left out, %d without run id\n", kept, excluded, noid > "/dev/stderr"
}' "${files[@]}" > "$tmp" || exit 1

chmod 664 "$tmp"
mv "$tmp" data/cache/runs.tsv
