
TMP=`mktemp /tmp/foo.XXXX`
./visualize -sample -g grammars/wmcfg/strauss.wmcfg -n 25 -o $TMP -seed 0 | `dirname $0`/canonicalize.sh
echo "======================================"
cat $TMP | egrep -v '\\item timestamp: '
rm $TMP
