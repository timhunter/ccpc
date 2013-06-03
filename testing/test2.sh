
TMP=`mktemp /tmp/foo.XXXX`
./visualize -kbest grammars/wmcfg/larsonian1.wmcfg 23 $TMP >/dev/null
cat $TMP | egrep -v '\\item timestamp: ' | egrep -v '^%'
rm $TMP
