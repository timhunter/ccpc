# For profiling, use: 'ocamlcp' instead of 'ocamlc'
#                     'ocamlopt -p' instead of 'ocamlopt'
# But profiling is incompatible with preprocessing so I'm leaving it aside.
COMPILER_BYTECODE=ocamlc
COMPILER_NATIVE=ocamlopt

FLAGS= -I .. -I ../mcfgread -I +ocamlgraph

# OCAMLINT= util.cmi nelist.cmi rule.cmi chart.cmi tables.cmi parser.cmi mcfgread/read.cmi util.cmi grammar.cmi derivation.cmi generate.cmi
OCAMLOBJ_nt= util.cmx nelist.cmx rule.cmx chart.cmx tables.cmx mcfgread/read.cmx mcfgread/lexer.cmx parser.cmx grammar.cmx derivation.cmx generate.cmx

all: kbest_demo

clean:
	rm -f *.o *.cmo *.cmi *.cmx
	rm -f kbest_demo

kbest_demo: kbest_demo.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ nums.cmxa unix.cmxa str.cmxa graph.cmxa $(OCAMLOBJ_nt) kbest_demo.cmx

kbest_demo.cmx: kbest_demo.ml
	$(COMPILER_NATIVE) $(FLAGS) -c kbest_demo.ml
