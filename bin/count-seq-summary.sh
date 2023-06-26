#! /usr/bin/env bash

# pass a ONT sequencing summary file and return pass/fail bases, reads and Nx (pass only)
# usage
# count-seq-summary.sh nx file

# get col index as they are not very consistent
pass=$(head -1 ${2} | sed 's/\t/\n/g' | nl | grep 'passes_filtering' | cut -f 1)
len=$(head -1 ${2} | sed 's/\t/\n/g' | nl | grep 'sequence_length_template' | cut -f 1)
qscore=$(head -1 ${2} | sed 's/\t/\n/g' | nl | grep 'mean_qscore_template' | cut -f 1)
#read -r pass len qscore < <(COMMAND)
# 

# bases \t reads
gawk -v nx="${1}" -v a="$pass" -v b="$len" -v c="$qscore" '
$a == "TRUE" {sum+=$b; count++; qscoresum+=$c; lenarray[idx++] = $b}
$a == "FALSE" {sum_f+=$b; count_f++; lenarray_f[idx++] = $b}


END {
    halfsum = sum * nx; halfsum_f = sum_f * nx;
    n = asort(lenarray); n_f = asort(lenarray_f);
    i = n; j = n_f;
    
    # Nx pass
    while (i >= 0) {
        if (cumsum <= halfsum) {
            cumsum += lenarray[i]
            i--
        } else {
            nxvalue = lenarray[i]
            break
        }
    }
    # Nx fail
    while (j >= 0) {
        if (cumsum_f <= halfsum_f) {
            cumsum_f += lenarray_f[j]
            j--
        } else {
            nxvalue_f = lenarray_f[j]
            break
        }
    }

    # file, bases_pass, bases_fail, reads_pass, reads_fail, Nx_pass, Nx_fail, mean_qscore
    printf "%s,%s,%s,%s,%s,%s,%s,%.2f\n", ARGV[1], sum, sum_f, count, count_f, nxvalue, nxvalue_f, qscoresum/count
    #printf "%s\n", nxvalue
}' "${2}"

