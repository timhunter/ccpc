
TMP=`mktemp /tmp/foo.XXXX`
./visualize -sample grammars/wmcfg/strauss.wmcfg 25 $TMP 0
echo "======================================"
cat $TMP | egrep -v '\\item timestamp: '
rm $TMP
