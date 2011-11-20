EXE=mcfg

# For profiling, use: 'ocamlcp' instead of 'ocamlc'
#                     'ocamlopt -p' instead of 'ocamlopt'
# But profiling is incompatible with preprocessing so I'm leaving it aside.
COMPILER_BYTECODE=ocamlc
COMPILER_NATIVE=ocamlopt

LEX=ocamllex
YACC=ocamlyacc

# Mattieu Guillaumin's Minimalist Grammar to Multiple Context-free Grammar translator
# NB: Guillaumin's code should be patched like this and then recompiled:
#     patch ../guillaumin/hmg2mcfg/hmgtransform.ml hmgtransform-fixity.patch
# pathname appropriatefor John's laptop
GUILLAUMIN=../bach-etal-replication/embed/guillaumin/hmg2mcfg/hmg2mcfg
#GUILLAUMIN=../guillaumin/hmg2mcfg/hmg2mcfg

FLAGS= -I mcfgread -I kbest -I +ocamlgraph
OCAMLOBJ_bc= util.cmo kbest/rational.cmo nelist.cmo rule.cmo mcfgread/read.cmo mcfgread/lexer.cmo chart.cmo tables.cmo parser.cmo grammar.cmo derivation.cmo generate.cmo 

OCAMLINT= util.cmi kbest/rational.cmi nelist.cmi rule.cmi chart.cmi tables.cmi parser.cmi mcfgread/read.cmi util.cmi grammar.cmi derivation.cmi generate.cmi
OCAMLOBJ_nt= util.cmx kbest/rational.cmx nelist.cmx rule.cmx chart.cmx tables.cmx mcfgread/read.cmx mcfgread/lexer.cmx parser.cmx grammar.cmx derivation.cmx generate.cmx

all: $(EXE)_nt train visualize

$(EXE)_bc: $(OCAMLINT) $(OCAMLOBJ_bc)  main.cmo
	$(COMPILER_BYTECODE) $(FLAGS) -o $@ nums.cma str.cma unix.cma graph.cma $(OCAMLOBJ_bc) main.cmo

$(EXE)_nt: $(OCAMLINT) $(OCAMLOBJ_nt) main.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ nums.cmxa str.cmxa unix.cmxa graph.cmxa $(OCAMLOBJ_nt) main.cmx

train: $(OCAMLINT) $(OCAMLOBJ_nt) train.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ nums.cmxa str.cmxa unix.cmxa graph.cmxa $(OCAMLOBJ_nt) train.cmx

visualize: $(OCAMLINT) $(OCAMLOBJ_nt) visualize.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ nums.cmxa unix.cmxa str.cmxa graph.cmxa $(OCAMLOBJ_nt) visualize.cmx

clean:
	rm -f *.o *.cmo *.cmi *.cmx
	rm -f mcfgread/*.o mcfgread/*.cmo mcfgread/*.cmi mcfgread/*.cmx
	rm -f kbest/*.o kbest/*.cmo kbest/*.cmi kbest/*.cmx
	rm -f $(EXE)_bc $(EXE)_nt train visualize

# the fig13.txt file is the sentence file with "whose--->who s" as appropriate for the Kaynian promotion analysis.

grammars/mcfgs/%.mcfg:	grammars/mg/%.pl $(GUILLAUMIN)
	$(GUILLAUMIN) -pl $< -o $@

grammars/mcfgs/%.dict:	grammars/mg/%.pl $(GUILLAUMIN)
	$(GUILLAUMIN) -pl $< -dict $@ -o /dev/null

grammars/wmcfg/%.wmcfg: grammars/mcfgs/%.mcfg %.train train
	./train grammars/mcfgs/$*.mcfg $*.train > $@

%.train:	%.train.annot stripcomment.sed blanks.grep killtrailingblanks.sed
	sed -E -f stripcomment.sed $< | sed -E -f killtrailingblanks.sed | egrep -v -f blanks.grep > $@

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
