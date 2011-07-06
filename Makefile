EXE=mcfg

#COMPILER_BYTECODE=ocamlc
COMPILER_BYTECODE=ocamlcp

#COMPILER_NATIVE=ocamlopt
COMPILER_NATIVE=ocamlopt -p

LEX=ocamllex
YACC=ocamlyacc

FLAGS= -I mcfgread -I kbest
OCAMLOBJ_bc= util.cmo kbest/rational.cmo nelist.cmo rule.cmo mcfgread/read.cmo mcfgread/lexer.cmo chart.cmo tables.cmo parser.cmo main.cmo

OCAMLINT= util.cmi kbest/rational.cmi nelist.cmi rule.cmi chart.cmi tables.cmi parser.cmi mcfgread/read.cmi util.cmi
OCAMLOBJ_nt= util.cmx kbest/rational.cmx nelist.cmx rule.cmx chart.cmx tables.cmx mcfgread/read.cmx mcfgread/lexer.cmx parser.cmx main.cmx

# There are some complicated interactions I don't understand yet between the native and bytecode compilation 
# procedures, such that if the bytecode exe is not produced *last*, there are "inconsistent assumptions over interface" 
# errors when things are loaded into the REPL.
all: $(EXE)_nt $(EXE)_bc

$(EXE)_bc: $(OCAMLINT) $(OCAMLOBJ_bc)
	$(COMPILER_BYTECODE) $(FLAGS) -o $@ $(OCAMLOBJ_bc)

$(EXE)_nt: $(OCAMLINT) $(OCAMLOBJ_nt) 
	$(COMPILER_NATIVE) $(FLAGS) -o $@ $(OCAMLOBJ_nt)


clean:
	rm -f *.o *.cmo *.cmi *.cmx mcfgread/*.o mcfgread/*.cmo mcfgread/*.cmi mcfgread/*.cmx kbest/*.o kbest/*.cmo kbest/*.cmi kbest/*.cmx $(EXE)_bc $(EXE)_nt

%.cmx: %.ml
	$(COMPILER_NATIVE) $(FLAGS) -c $*.ml

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
