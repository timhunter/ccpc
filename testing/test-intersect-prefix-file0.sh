TMP=`mktemp`

# write FSA file corresponding to the prefix "Jon hit"
cat <<EOF > $TMP
0	1	Jon	1.0
1	2	hit	1.0
2	2	hit	1.0
2	2	Jon	1.0
2	2	dog	1.0
2	2	stick	1.0
2	2	with	1.0
2	2	the	1.0
2	2	<epsilon>	1.0
2
EOF

# grep command omits the comment line, because the name of the file changes each run
./mcfg_nt grammars/wmcfg/strauss.wmcfg -intersect -file $TMP | grep -v $TMP

rm $TMP
