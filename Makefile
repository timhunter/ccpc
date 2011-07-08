EXE=mcfg

# For profiling, use: 'ocamlcp' instead of 'ocamlc'
#                     'ocamlopt -p' instead of 'ocamlopt'
# But profiling is incompatible with preprocessing so I'm leaving it aside.
COMPILER_BYTECODE=ocamlc
COMPILER_NATIVE=ocamlopt

LEX=ocamllex
YACC=ocamlyacc

FLAGS= -I mcfgread -I kbest
OCAMLOBJ_bc= util.cmo kbest/rational.cmo nelist.cmo rule.cmo mcfgread/read.cmo mcfgread/lexer.cmo chart.cmo tables.cmo parser.cmo

OCAMLINT= util.cmi kbest/rational.cmi nelist.cmi rule.cmi chart.cmi tables.cmi parser.cmi mcfgread/read.cmi util.cmi
OCAMLOBJ_nt= util.cmx kbest/rational.cmx nelist.cmx rule.cmx chart.cmx tables.cmx mcfgread/read.cmx mcfgread/lexer.cmx parser.cmx

# There are some complicated interactions I don't understand yet between the native and bytecode compilation 
# procedures, such that if the bytecode exe is not produced *last*, there are "inconsistent assumptions over interface" 
# errors when things are loaded into the REPL.
all: $(EXE)_nt $(EXE)_bc

$(EXE)_bc: $(OCAMLINT) $(OCAMLOBJ_bc) main.cmo
	$(COMPILER_BYTECODE) $(FLAGS) -o $@ $(OCAMLOBJ_bc) main.cmo

$(EXE)_nt: $(OCAMLINT) $(OCAMLOBJ_nt) main.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ $(OCAMLOBJ_nt) main.cmx

train: $(OCAMLINT) $(OCAMLOBJ_bc) train.cmo
	$(COMPILER_BYTECODE) $(FLAGS) -o $@ $(OCAMLOBJ_bc) train.cmo

clean:
	rm -f *.o *.cmo *.cmi *.cmx mcfgread/*.o mcfgread/*.cmo mcfgread/*.cmi mcfgread/*.cmx kbest/*.o kbest/*.cmo kbest/*.cmi kbest/*.cmx $(EXE)_bc $(EXE)_nt

debug.cmo: debug.ml
	$(COMPILER_BYTECODE) -c -I +camlp5 -pp 'camlp5o pa_extend.cmo q_MLast.cmo -loc loc' -dtypes debug.ml

%.cmx: %.ml debug.cmo
	$(COMPILER_NATIVE) $(FLAGS) -pp 'camlp5o -I . pr_o.cmo debug.cmo' -dtypes -c $*.ml

%.cmo: %.ml debug.cmo
	$(COMPILER_BYTECODE) $(FLAGS) -pp 'camlp5o -I . pr_o.cmo debug.cmo' -dtypes -c $*.ml

%.cmi: %.mli
	$(COMPILER_BYTECODE) $(FLAGS) -c $*.mli

%.ml: %.mll
	$(LEX) $*.mll

%.mli: %.mly
	$(YACC) $*.mly

%.ml: %.mly
	$(YACC) $*.mly
