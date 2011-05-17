
ml_targets 	= runner.ml
mli_targets 	= runner.mli
targets 	= $(mli_targets) $(ml_targets)

%.svg: 	%.dot
	dot -Tsvg -o$*.svg $*.dot

all:	compile html.doc man.doc
	$(NOOP)

compile: 	$(mli_targets) $(ml_targets)
	ocamlc -c $(mli_targets) $(ml_targets)

html.doc:	$(targets)
	mkdir -p html
	ocamldoc -html -d html $(targets)

man.doc: 	$(targets)
	mkdir -p man
	ocamldoc -man -man-mini -d man $(targets)

clean:
	rm -f *.tmp* *.svg *.dot *.cmi *.cmo man/* html/*