
TMP=`mktemp /tmp/foo.XXXX`
./visualize -sample grammars/wmcfg/strauss.wmcfg 33 $TMP 0
echo "======================================"
cat $TMP | egrep -v '\\item timestamp: '
rm $TMP
