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
#GUILLAUMIN=../bach-etal-replication/embed/guillaumin/hmg2mcfg/hmg2mcfg
GUILLAUMIN=../guillaumin/hmg2mcfg/hmg2mcfg

# Default to the grammars directory inside this directory.
# Any definition of GRAMMARS given on the command line will override this.
GRAMMARS=grammars

FLAGS= -I mcfgread -I +ocamlgraph
OCAMLOBJ_bc= util.cmo fsa.cmo nelist.cmo rule.cmo mcfgread/read.cmo mcfgread/lexer.cmo chart.cmo tables.cmo parser.cmo grammar.cmo derivation.cmo generate.cmo path.cmo

OCAMLINT= util.cmi fsa.cmi nelist.cmi rule.cmi chart.cmi tables.cmi parser.cmi mcfgread/read.cmi util.cmi grammar.cmi derivation.cmi generate.cmi path.cmi
OCAMLOBJ_nt= util.cmx fsa.cmx nelist.cmx rule.cmx chart.cmx tables.cmx mcfgread/read.cmx mcfgread/lexer.cmx parser.cmx grammar.cmx derivation.cmx generate.cmx path.cmx

all: $(EXE)_nt train visualize cycles compare

$(EXE)_bc: $(OCAMLINT) $(OCAMLOBJ_bc)  main.cmo
	$(COMPILER_BYTECODE) $(FLAGS) -o $@ nums.cma str.cma unix.cma graph.cma $(OCAMLOBJ_bc) main.cmo

$(EXE)_nt: $(OCAMLINT) $(OCAMLOBJ_nt) main.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ nums.cmxa str.cmxa unix.cmxa graph.cmxa $(OCAMLOBJ_nt) main.cmx

train: $(OCAMLINT) $(OCAMLOBJ_nt) train.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ nums.cmxa str.cmxa unix.cmxa graph.cmxa $(OCAMLOBJ_nt) train.cmx

visualize: $(OCAMLINT) $(OCAMLOBJ_nt) visualize.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ nums.cmxa unix.cmxa str.cmxa graph.cmxa $(OCAMLOBJ_nt) visualize.cmx

cycles: $(OCAMLINT) $(OCAMLOBJ_nt) cycles.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ nums.cmxa unix.cmxa str.cmxa graph.cmxa $(OCAMLOBJ_nt) cycles.cmx

compare: $(OCAMLINT) $(OCAMLOBJ_nt) compare.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ nums.cmxa unix.cmxa str.cmxa graph.cmxa $(OCAMLOBJ_nt) compare.cmx

clean:
	rm -f *.o *.cmo *.cmi *.cmx
	rm -f mcfgread/*.o mcfgread/*.cmo mcfgread/*.cmi mcfgread/*.cmx
	rm -f kbest/*.o kbest/*.cmo kbest/*.cmi kbest/*.cmx
	rm -f $(EXE)_bc $(EXE)_nt train visualize cycles

# the fig13.txt file is the sentence file with "whose--->who s" as appropriate for the Kaynian promotion analysis.

# For reasons I do not understand, make thinks that mcfg files are ``intermediate'' 
# and should be deleted (even though they are mentioned explicitly).
# Anyway, this line stops it from deleting intermediate files.
.SECONDARY:

$(GRAMMARS)/mcfgs/%.mcfg: $(GRAMMARS)/mg/%.pl $(GUILLAUMIN)
	$(GUILLAUMIN) -pl $< -o $@

$(GRAMMARS)/mcfgs/%.dict: $(GRAMMARS)/mg/%.pl $(GUILLAUMIN)
	$(GUILLAUMIN) -pl $< -dict $@ -o /dev/null

define weights_from_corpus
    @echo "*** Makefile: Using training corpus $(CORPUS)"
    ./train $(GRAMMARS)/mcfgs/$*.mcfg $(CORPUS) > $@
endef

define weights_uniform
    @echo "*** Makefile: Defaulting to uniform distribution"
    awk -f uniform.awk $(GRAMMARS)/mcfgs/$*.mcfg > $@
endef

$(GRAMMARS)/wmcfg/%.wmcfg: $(GRAMMARS)/mcfgs/%.mcfg train
	$(eval CORPUS=$(shell ./find_training_file.sh $(GRAMMARS)/mcfgs/$*.mcfg))
	$(if $(CORPUS), $(weights_from_corpus), $(weights_uniform))

%.train:	%.train.annot stripcomment.sed blanks.grep killtrailingblanks.sed
	sed -E -f stripcomment.sed $< | sed -E -f killtrailingblanks.sed | egrep -v -f blanks.grep > $@

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
