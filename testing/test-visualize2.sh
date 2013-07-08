
TMP=`mktemp /tmp/foo.XXXX`
./visualize -sample grammars/wmcfg/strauss.wmcfg 25 $TMP 0 | `dirname $0`/canonicalize.sh
echo "======================================"
cat $TMP | egrep -v '\\item timestamp: '
rm $TMP
