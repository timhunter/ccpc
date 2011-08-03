EXE=mcfg

# For profiling, use: 'ocamlcp' instead of 'ocamlc'
#                     'ocamlopt -p' instead of 'ocamlopt'
# But profiling is incompatible with preprocessing so I'm leaving it aside.
COMPILER_BYTECODE=ocamlc
COMPILER_NATIVE=ocamlopt

LEX=ocamllex
YACC=ocamlyacc

FLAGS= -I mcfgread -I kbest
OCAMLOBJ_bc= util.cmo kbest/rational.cmo nelist.cmo rule.cmo mcfgread/read.cmo mcfgread/lexer.cmo chart.cmo tables.cmo parser.cmo grammar.cmo derivation.cmo

OCAMLINT= util.cmi kbest/rational.cmi nelist.cmi rule.cmi chart.cmi tables.cmi parser.cmi mcfgread/read.cmi util.cmi grammar.cmi derivation.cmi
OCAMLOBJ_nt= util.cmx kbest/rational.cmx nelist.cmx rule.cmx chart.cmx tables.cmx mcfgread/read.cmx mcfgread/lexer.cmx parser.cmx grammar.cmx derivation.cmx

all: $(EXE)_nt train

$(EXE)_bc: $(OCAMLINT) $(OCAMLOBJ_bc) main.cmo
	$(COMPILER_BYTECODE) $(FLAGS) -o $@ $(OCAMLOBJ_bc) main.cmo

$(EXE)_nt: $(OCAMLINT) $(OCAMLOBJ_nt) main.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ $(OCAMLOBJ_nt) main.cmx

train: $(OCAMLINT) $(OCAMLOBJ_nt) train.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ $(OCAMLOBJ_nt) train.cmx

clean:
	rm -f *.o *.cmo *.cmi *.cmx
	rm -f mcfgread/*.o mcfgread/*.cmo mcfgread/*.cmi mcfgread/*.cmx
	rm -f kbest/*.o kbest/*.cmo kbest/*.cmi kbest/*.cmx
	rm -f $(EXE)_bc $(EXE)_nt train

# I am trying here to automate the process, end to end

# added by John; start from Stabler's revision of larsonian grammar
GUILLAUMIN=../bach-etal-replication/embed/guillaumin/hmg2mcfg/hmg2mcfg
larsonian1.mcfg: grammars/mg/larsonian1.pl
	$(GUILLAUMIN) -pl grammars/mg/larsonian1.pl -o grammars/mcfgs/larsonian1.mcfg

# my post-thesis revision
lstack.mcfg: grammars/mg/larsonian-stackable.pl
	$(GUILLAUMIN) -pl grammars/mg/larsonian-stackable.pl -o grammars/mcfgs/lstack.mcfg

sleep.mcfg: grammars/mg/sleep.pl
	$(GUILLAUMIN) -pl grammars/mg/sleep.pl -o grammars/mcfgs/sleep.mcfg

# John: for some reason I could not get output redirection to sent the result immediately to the right directory
# the fig13.txt file is the sentence file with "whose--->who s" as appropriate for the Kaynian promotion analysis.
# and counts from Figure 13 of the Cog Sci article, ie from the Brown corpus
larsonian1.wmcfg: larsonian1.mcfg
	./train grammars/mcfgs/larsonian1.mcfg fig13.txt > larsonian1.wmcfg
	mv larsonian1.wmcfg grammars/wmcfg/

lstack.wmcfg: lstack.mcfg
	./train grammars/mcfgs/lstack.mcfg fig13.txt > lstack.wmcfg
	mv lstack.wmcfg grammars/wmcfg/

sleep.wmcfg: sleep.mcfg
	./train grammars/mcfgs/sleep.mcfg johnmary.txt > sleep.wmcfg
	mv sleep.wmcfg grammars/wmcfg/

debug.cmo: debug.ml
	$(COMPILER_BYTECODE) -c -I +camlp5 -pp 'camlp5o pa_extend.cmo q_MLast.cmo -loc loc' debug.ml

%.cmx: %.ml debug.cmo
	$(COMPILER_NATIVE) $(FLAGS) -pp 'camlp5o -I . debug.cmo' -c $*.ml

%.cmo: %.ml debug.cmo
	$(COMPILER_BYTECODE) $(FLAGS) -pp 'camlp5o -I . debug.cmo' -c $*.ml

%.cmi: %.mli
	$(COMPILER_BYTECODE) $(FLAGS) -c $*.mli

%.ml: %.mll
	$(LEX) $*.mll

%.mli: %.mly
	$(YACC) $*.mly

%.ml: %.mly
	$(YACC) $*.mly
