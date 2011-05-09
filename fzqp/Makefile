
ml_targets 	= runner.ml
mli_targets 	= runner.mli
targets 	= $(mli_targets) $(ml_targets)

all:	compile html.doc man.doc
	$(NOOP)
compile: 	$(mli_targets)
	ocamlc -c $(mli_targets)

html.doc:	$(targets)
	ocamldoc -html -d html $(targets)

man.doc: 	$(targets)
	ocamldoc -man -man-mini -d man $(targets)