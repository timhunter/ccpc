
TMP=`mktemp /tmp/foo.XXXX`
./visualize -sample grammars/wmcfg/larsonian1.wmcfg 20 $TMP 0 >/dev/null
cat $TMP | egrep -v '\\item timestamp: '
rm $TMP
