
TMP=`mktemp /tmp/foo.XXXX`
./visualize -kbest -g grammars/wmcfg/strauss.wmcfg -n 33 -o $TMP | `dirname $0`/canonicalize.sh
echo "======================================"
cat $TMP | egrep -v '\\item timestamp: '
rm $TMP
