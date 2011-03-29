OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLFLAGS=-g
OCAMLOPTFLAGS= -inline 32 -ccopt "-O3"

TARGET=mcfgread

all: grammar mcfgread

mcfgread: lexer.cmo parse.cmo main.cmo grammar.cmo
	$(OCAMLC) $(OCAMLFLAGS) -o $@ lexer.cmo parse.cmo main.cmo grammar.cmo

grammar: grammar.ml
	$(OCAMLC) $(OCAMLFLAGS) -c grammar.ml

depend:
	ocamldep *.ml *.mli > .depend

clean:
	rm -rf *.cmi *.cmo *.cmx *.o $(TARGET)

.SUFFIXES: .ml .mli .mll .mly .cmo .cmi

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<
.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<
.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<
.mll.ml:
	ocamllex $<
.mly.ml:
	ocamlyacc $<
.mly.mli:
	ocamlyacc $<

include .depend
