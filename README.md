# Overview
This Shiny app reads and summarizes ONT data by reading the `final_summary*` and `sequence_summary*` files.
The package contains data processing scripts, data folder, and a Shiny app.
Code is available at https://github.com/angelovangel/ont-usage   
Dependencies are managed with `renv`.

# Implementation details
The scripts needed for fetching and processing data are under `bin`.


# Deployment

```
cd ~/code/ont-usage && echo $(date) >> logs && ./bin/get-files.sh >> logs
```
