EXE=mcfg

#COMPILER_BYTECODE=ocamlc
COMPILER_BYTECODE=ocamlcp

#COMPILER_NATIVE=ocamlopt
COMPILER_NATIVE=ocamlopt -p

$(EXE)_bc: util.cmo rule.cmo deriver.cmo parser.cmo main.cmo
	$(COMPILER_BYTECODE) -o $@ $^

$(EXE)_nt: util.cmx rule.cmx deriver.cmx parser.cmx main.cmx
	$(COMPILER_NATIVE) -o $@ $^

clean:
	rm -f *.o *.cmo *.cmi *.cmx $(EXE)_bc $(EXE)_nt $(EXE)_new

%.cmx: %.ml
	$(COMPILER_NATIVE) -c $*.ml

%.cmo: %.ml
	$(COMPILER_BYTECODE) -c $*.ml
