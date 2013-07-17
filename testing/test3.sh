
TMP=`mktemp /tmp/foo.XXXX`
./visualize -sample -g grammars/wmcfg/larsonian1.wmcfg -n 20 -o $TMP -seed 0 >/dev/null
cat $TMP | egrep -v '\\item timestamp: ' | egrep -v '^%'
rm $TMP
