
TMP=`mktemp /tmp/foo.XXXX`
./visualize -kbest grammars/wmcfg/strauss.wmcfg 33 $TMP
echo "======================================"
cat $TMP | egrep -v '\\item timestamp: '
rm $TMP
