
# Just use the first 12 lines of the training file, because the whole thing is too slow.
# And add in a line that the grammar will not be able to parse.
./train grammars/mcfgs/chomskyan.mcfg <(echo "1 hello world" ; head -12 grammars/train/chomskyan.train)

