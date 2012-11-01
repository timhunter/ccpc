
TMP=`mktemp /tmp/foo.XXXX`
./visualize -kbest grammars/wmcfg/larsonian1.wmcfg 20 $TMP >/dev/null
cat $TMP | egrep -v '\\item timestamp: '
rm $TMP
