#!/bin/bash
# Run NounPhrase repeatedly to recover from running out of memory

outfile=NounPhrase.out

until ./Explore +RTS -N4 -s; do
    date
    wc "$outfile"*
    cat "$outfile" >> "$outfile".1
    echo
done
