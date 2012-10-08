
TMP=`mktemp /tmp/foo.XXXX`
./visualize -kbest chinesepromotion.Vt.global.chart 20 $TMP >/dev/null
cat $TMP | egrep -v '\\item timestamp: '
rm $TMP
