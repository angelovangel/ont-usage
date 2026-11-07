# Overview
This Shiny app shows the usage and output of the ONT (PromethION, GridION) and PacBio sequencers.
ONT data comes from the `final_summary*` and `sequencing_summary*` files of each run, PacBio data from a SMRT Link database dump.
The repo contains the data processing scripts and the app, the data itself is not tracked.  
Code is available at https://github.com/angelovangel/ont-usage  
Dependencies are managed with `renv`.

# App structure
The app is a `shinydashboard` app with a login (`shinymanager`). It only reads the files in `data/app/`, and loads them once per session.

| File | Purpose |
|---|---|
| `ui.R` | Layout, one tab per view |
| `server.R` | Loads the data, filters it and renders the outputs, in the same order as the tabs |
| `R/global.R` | Packages and shared helpers (colors, value box tables, merging overlapping runs) |
| `bin/siformat.sh` | Number formatting with k, M, G suffixes, needs `ksh`. Used by the app and by the data build |

| Tab | Shows |
|---|---|
| ONT usage | Timeline of runs, run hours and % usage, table of runs |
| ONT output | Flow cells, bases and reads over time, flow cell re-use |
| PacBio usage | Timeline of runs, run hours and % usage, table of runs |
| PacBio output | SMRT cells, CCS bases and reads over time |
| PacBio status | Live state of the instruments, from SMRT Link |

# Data pipeline
`bin/get-files.sh` runs all steps below, one run at a time (lock file `data/.get-files.lock`). It prints `=== Done` only if everything needed for the app succeeded.

1. `rsync` copies the summary files from the storage to `data/raw/`
2. `bin/parse-final-summaries.sh` makes one row per `final_summary` file in `data/cache/runs.tsv`. Runs on `CTC` flow cells or with sample `no_sample` are left out
3. `bin/update-counts.sh` counts the sequencing summaries these runs refer to, using `bin/count-one.sh` and `bin/count-seq-summary.sh`
4. `bin/pb-dump.R` dumps the PacBio data to `data/raw/pb_dump.csv`. If this fails the last dump is used
5. `bin/build-app-data.R` joins everything, runs the checks and writes `data/app/`

## Count cache
Counting needs to read files of up to tens of GB, so each result is kept in `data/cache/counts/`, one file per sequencing summary. A file is counted again only if its size, its modification time or `COUNT_VERSION` in `bin/count-seq-summary.sh` changes. Changing `COUNT_VERSION` recounts all files, which takes many hours.

The disk is the bottleneck, so files are counted one at a time. `COUNT_JOBS` sets the number of parallel jobs, the default is 1.

Failed files are not cached and are retried on the next run, see `data/count.joblog`. Summaries of runs without basecalling have no pass/fail, length or qscore columns. They are not failures, they are cached and shown as "not basecalled".

## How runs are built
- A run is one `protocol_run_id`. If it is in more than one directory, the copy with the most pass bases is used
- The PI code (e.g. `P12U4`) is read from `protocol_group_id`, then from the directory names. `POUO` is read as `P0U0`, and plasmid runs without a code are `P0U0`
- `config/run_overrides.tsv` sets the PI of runs where this does not work
- The build reports runs without a PI, with a PI not in `pis.tsv`, not basecalled and without counts

## Checks
`bin/build-app-data.R` does not change `data/app/` if any of these fail, and the app keeps showing the previous data:

- required columns are missing, run ids are duplicated, or runs have no start or end
- more than 10% of the runs have no counts
- the number of runs or PacBio rows goes down
- the total of pass bases goes down by more than 1%

After removing runs on purpose, publish with `bin/build-app-data.R --force`.

# Directories
```
config/                     not tracked, except the example
  pis.tsv                   pi, pi_name, pi_portal, division
  credentials.rds           app logins
  run_overrides.tsv         optional: run_id, pi, note. See run_overrides.example.tsv
data/                       not tracked
  raw/prom/, raw/grid/      summary files from rsync
  raw/pb_dump.csv           PacBio dump
  cache/runs.tsv            one row per final_summary file
  cache/counts/             count cache, one file per sequencing summary
  cache/counts.tsv          count cache as one table
  counts.csv                counted files, as one table
  app/                      the only files the app reads
```

# Deployment
The app runs on the same machine as the pipeline. SMRT Link access is set in `.Renviron` in the project directory (not tracked), with `SMRT_BASE`, `SMRT_USER` and `SMRT_PASS`.

Run the pipeline with cron:
```
0 2 * * * cd /home/angeloas/code/ont-usage && bin/get-files.sh 2>&1 | tee -a logs
```

Use `grep -a` on `logs`. Don't pull changes on the server while a run is going.
