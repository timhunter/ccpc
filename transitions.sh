#!/bin/bash

# Given a grammar and a full sentence, this script runs visualization of 
# the intersection of the grammar with each prefix of the sentence. 
# It produces (a) a pdf file for each prefix, and
#             (b) a tex file bringing together the tables of surfaces strings 
#                 at each prefix.

function print_usage_exit () {
        echo "Usage: $0 <mode> <grammar-file> <sentence> <number-of-trees> <tag>"
        echo "       where <mode> is either '-sample' or '-kbest'"
        exit 1
}

if [ $# -ne 5 ] ; then
        print_usage_exit
fi

mode=$1
grammar=$2
sentence=$3
num_trees=$4
tag=$5

if [ "$mode" != "-kbest" ] && [ "$mode" != "-sample" ] ; then
        print_usage_exit
fi

if [ ! -f $grammar ] ; then
        echo "File $grammar does not exist"
        exit 1
fi

echo "============================"
echo "Starting `basename $0`"
echo "Grammar is: $grammar"
echo "Sentence is: $sentence"
echo "============================"

function get_prefixes () {
        awk '
                {
                        for (i=0; i<=NF; i++) {
                                prefix="";
                                for (j=1; j<=i; j++) {
                                        prefix = prefix " " $j;
                                }
                                sub(/^ /, "", prefix);
                                print prefix;
                        }
                }
        '
}

function get_tables () {
        awk '
                BEGIN { x=0 }
                {
                        if ($0 ~ /\\begin{table}/) {x=1;}
                        if (x==1) {print $0;}
                        if ($0 ~ /\\end{table}/) {x=0; print "";}
                }
        '
}

function no_spaces () {
        echo $1 | sed 's/ /-/g'
}

function compute_ERs () {
        awk -F'\t' '
                BEGIN { last = -1 }
                {
                        entropy_here = $1 ;
                        if (last == -1) {
                            er_string = "none yet"
                        } else {
                            er = (last > entropy_here) ? (last - entropy_here) : 0
                            er_string = sprintf("%f",er)
                            total_er += er
                        }
                        printf("ER: %s \tentropy: %f \tprefix: %s\n", er_string, entropy_here, $2)
                        last = entropy_here ;
                }
                END { printf("Total ER: %f\n", total_er) }
        '
}

tables_file=/tmp/`basename $grammar .wmcfg`.$tag.`no_spaces "$sentence"`.tables.$$.tex
entropies_file=/tmp/`basename $grammar .wmcfg`.$tag.`no_spaces "$sentence"`.entropies.$$.tex
combined_file=`basename $grammar .wmcfg`.$tag.`no_spaces "$sentence"`.combined.$$.tex

renormalizer=./renormalize.m

echo "$sentence" | get_prefixes |\
while read prefix ; do
        id=/tmp/`basename $grammar .wmcfg`.$tag.`no_spaces "$prefix"`.$$
        ./intersect -g $grammar -prefix "$prefix" > $id.chart
        $renormalizer $id.chart | sed -f twospaces.sed > $id.global.chart # sed call used to be in wrapper, which we no longer use
        echo -e "`egrep -o "entropy = [-+]?[0-9]*\.?[0-9]*([eE][-+]?[0-9]+)?" $id.global.chart | cut -d ' ' -f 3` \t $prefix" >> $entropies_file
        if [ "$mode" == "-sample" ] ; then
            seed_arg="-seed $$"  # use $$, which also appears in output filenames, as random seed
        else
            seed_arg=""
        fi
        ./visualize $mode -g $id.global.chart -n $num_trees -o $id.tex $seed_arg >/dev/null
        pdflatex $id.tex >/dev/null
        echo "*** Created pdf file: `basename $id`.pdf"
        cat $id.tex | get_tables >> $tables_file
        rm -f $id.chart $id.global.chart $id.tex `basename $id`.aux `basename $id`.log
done

# Now construct the final combined tex file
echo "\\documentclass{article}" >> $combined_file
echo "\\usepackage{fullpage}"   >> $combined_file
echo "\\begin{document}"        >> $combined_file
echo ""                         >> $combined_file
cat $tables_file >> $combined_file
echo "\\begin{verbatim}" >> $combined_file
cat $entropies_file | compute_ERs | tee -a $combined_file
echo "\\end{verbatim}" >> $combined_file
echo ""                         >> $combined_file
echo "\\end{document}" >> $combined_file

rm $tables_file
rm $entropies_file

echo "============================"
echo "Summary info is collected in $combined_file"
echo "Exiting `basename $0`"
echo "============================"

