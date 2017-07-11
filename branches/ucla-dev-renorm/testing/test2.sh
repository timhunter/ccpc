
TMP=`mktemp /tmp/foo.XXXX`
./visualize -kbest -g grammars/wmcfg/larsonian1.wmcfg -n 23 -o $TMP >/dev/null
cat $TMP | egrep -v '\\item timestamp: ' | egrep -v '^%'
rm $TMP
