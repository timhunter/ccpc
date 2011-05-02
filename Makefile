EXE=mcfg

#COMPILER_BYTECODE=ocamlc
COMPILER_BYTECODE=ocamlcp

#COMPILER_NATIVE=ocamlopt
COMPILER_NATIVE=ocamlopt -p

LEX=ocamllex
YACC=ocamlyacc

FLAGS= -I mcfgread
OCAMLINT_bc= util.cmo nelist.cmi rule.cmi parser.cmi mcfgread/read.cmi util.cmi 
OCAMLOBJ_bc= util.cmo nelist.cmo rule.cmo mcfgread/read.cmo mcfgread/lexer.cmo parser.cmo main.cmo

OCAMLINT_nt= util.cmx nelist.cmi rule.cmi parser.cmi mcfgread/read.cmi util.cmi 
OCAMLOBJ_nt= util.cmx nelist.cmx rule.cmx mcfgread/read.cmx mcfgread/lexer.cmx deriver.cmx parser.cmx main.cmx


$(EXE)_bc: $(OCAMLINT_bc) $(OCAMLOBJ_bc)
	$(COMPILER_BYTECODE) $(FLAGS) -o $@ $(OCAMLOBJ_bc)

$(EXE)_nt: $(OCAMLINT_nt) $(OCAMLOBJ_nt) 
	$(COMPILER_NATIVE) $(FLAGS) -o $@ $(OCAMLOBJ_nt)

tree: 
	echo hello > #!/bin/bash

clean:
	rm -f *.o *.cmo *.cmi *.cmx mcfgread/*.o mcfgread/*.cmo mcfgread/*.cmi mcfgread/*.cmx $(EXE)_bc $(EXE)_nt

%.cmx: %.ml
	$(COMPILER_NATIVE) $(FLAGS) -c $*.ml

%.cmo: %.ml
	$(COMPILER_BYTECODE) $(FLAGS) -c $*.ml
	
%.cmi: %.mli
	$(COMPILER_NATIVE) $(FLAGS) -c $*.mli

%.ml: %.mll
	$(LEX) $*.mll

%.mli: %.mly
	$(YACC) $*.mly

%.ml: %.mly
	$(YACC) $*.mly
