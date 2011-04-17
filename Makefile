EXE=mcfg

#COMPILER_BYTECODE=ocamlc
COMPILER_BYTECODE=ocamlcp

#COMPILER_NATIVE=ocamlopt
COMPILER_NATIVE=ocamlopt -p

LEX=ocamllex
YACC=ocamlyacc

FLAGS= -I mcfgread
OCAMLINT= util.cmo nelist.cmi rule.cmi parser.cmi mcfgread/read.cmi util.cmi 
OCAMLOBJ= util.cmo nelist.cmo rule.cmo mcfgread/read.cmo mcfgread/lexer.cmo deriver.cmo parser.cmo main.cmo



$(EXE)_bc: $(OCAMLINT) $(OCAMLOBJ)
	$(COMPILER_BYTECODE) $(FLAGS) -o $@ $(OCAMLOBJ)

$(EXE)_nt: util.cmx rule.cmx deriver.cmx parser.cmx main.cmx
	$(COMPILER_NATIVE) -o $@ $^

clean:
	rm -f *.o *.cmo *.cmi *.cmx mcfgread/*.o mcfgread/*.cmo mcfgread/*.cmi mcfgread/*.cmx $(EXE)_bc $(EXE)_nt

%.cmx: %.ml
	$(COMPILER_NATIVE) -c $*.ml

%.cmo: %.ml
	$(COMPILER_BYTECODE) $(FLAGS) -c $*.ml
	
%.cmi: %.mli
	$(COMPILER_BYTECODE) $(FLAGS) -c $*.mli

%.ml: %.mll
	$(LEX) $*.mll

%.mli: %.mly
	$(YACC) $*.mly

%.ml: %.mly
	$(YACC) $*.mly
