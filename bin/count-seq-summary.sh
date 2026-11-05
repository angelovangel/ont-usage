#! /usr/bin/env bash

# pass a ONT sequencing summary file and return pass/fail bases, reads and Nx (pass only)
# usage
# count-seq-summary.sh nx file
# count-seq-summary.sh --version

# bump when the output for the same file changes, this makes the count cache recount every file
COUNT_VERSION=1
[ "$1" = "--version" ] && { echo "$COUNT_VERSION"; exit 0; }

# get col index as they are not very consistent
pass=$(head -1 ${2} | sed 's/\t/\n/g' | nl | grep 'passes_filtering' | cut -f 1)
len=$(head -1 ${2} | sed 's/\t/\n/g' | nl | grep 'sequence_length_template' | cut -f 1)
qscore=$(head -1 ${2} | sed 's/\t/\n/g' | nl | grep 'mean_qscore_template' | cut -f 1)
#read -r pass len qscore < <(COMMAND)
# 
# exit early if something is wrong with the file
[ "$pass" -eq "$pass" ] || exit 1

# bases \t reads
gawk -v nx="${1}" -v a="$pass" -v b="$len" -v c="$qscore" '
# Nx - length of the read at which the cumulative sum (longest first) reaches target
# hist is read length -> number of reads, so memory depends on distinct lengths, not on reads
function get_nx(hist, target,    len, cumsum) {
    PROCINFO["sorted_in"] = "@ind_num_desc"
    for (len in hist) {
        cumsum += len * hist[len]
        if (cumsum >= target) {
            return len
        }
    }
}

$a == "TRUE" {sum+=$b; count++; qscoresum+=$c; hist[$b]++}
$a == "FALSE" {sum_f+=$b; count_f++; hist_f[$b]++}


END {
    if (qscoresum > 0) {
        meanqscore = qscoresum/count
    } else {
        meanqscore = 0
    }

    nxvalue = get_nx(hist, sum * nx)
    nxvalue_f = get_nx(hist_f, sum_f * nx)

    # file, bases_pass, bases_fail, reads_pass, reads_fail, Nx_pass, Nx_fail, mean_qscore
    printf "%s,%s,%s,%s,%s,%s,%s,%.2f\n", ARGV[1], sum, sum_f, count, count_f, nxvalue, nxvalue_f, meanqscore
    #printf "%s\n", nxvalue
}' "${2}"

