#!/bin/bash

function plot () {

    input=$1
    output=`basename $input .tex`.png
    datfile=`mktemp`

    ### Data file needs to look like this
    ### # Word  Ent ER
    ### -       4.7 0.0
    ### N       3.7 1.0
    ### Acc     4.8 0.0
    ### Vt      4.8 0.0
    ### N       5.2 0.0

    grep ER $input | sed 's/none yet/0/;s/prefix:  $/prefix:  -/' | awk '/^ER:/ {print $NF, $4, $2}' > $datfile

    total_er=`awk '/^Total ER:/ {print $3}' $input`
    #title=`echo $input | sed 's/\.combined\(\.[0-9]*\)\.tex$/\1/'`
    title=`echo $input | sed -r 's/\.[a-zA-Z-]+\.combined(\.[0-9]*)\.tex$/\1/'`
    title="$title\nTotal ER: $total_er"

    gnuplot <<-EOF   # The dash lets us indent the here-document by causing leading tabs to be ignored

		set yrange [0:$upper_bound]

		set style fill solid border -1
		set style histogram rowstacked gap 1
		set style data histograms
		set title "$title"
		set terminal png large size 400,300 ; set output "$output"
		plot '$datfile' u 2:xtic(1) lc rgb "white" t "", \
		     ""         u 3         lc rgb "gray" t ""

	EOF

    rm $datfile

    echo $output

}

highest_entropy=`egrep -h "^ER" $@ | sed -r 's/.*entropy: ([0-9]+\.[0-9]+).*/\1/g' | sort -rn | head -1`
upper_bound=`echo "$highest_entropy + 1" | bc`

PNG_FILES=""
for f in "$@" ; do
    result=`plot $f`
    PNG_FILES="$PNG_FILES $result"
done

rowwidth=2
numfiles=$#
numrows=`echo "($numfiles / $rowwidth) + ($numfiles % $rowwidth)" | bc`   # calculation only perfect for rowwidth=2, but not too bad in other cases
montage -tile ${rowwidth}x${numrows} -geometry 400x300+10x10 $PNG_FILES out.png
rm $PNG_FILES

