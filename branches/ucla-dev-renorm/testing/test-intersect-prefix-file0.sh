TMP=`mktemp /tmp/tmp.XXXXXX`

# write FSA file corresponding to the prefix "Jon hit"
cat <<EOF > $TMP
0	1	Jon	0
1	2	hit	0
2	2	hit	0
2	2	Jon	0
2	2	dog	0
2	2	stick	0
2	2	with	0
2	2	the	0
2	2	<epsilon>	0
2
EOF

# grep command omits the comment line, because the name of the file changes each run
./intersect -g grammars/wmcfg/strauss.wmcfg -file $TMP | grep -v $TMP

rm $TMP
