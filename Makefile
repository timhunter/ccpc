# For profiling, use: 'ocamlcp' instead of 'ocamlc'
#                     'ocamlopt -p' instead of 'ocamlopt'
# But profiling is incompatible with preprocessing so I'm leaving it aside.
COMPILER_BYTECODE=ocamlc
COMPILER_NATIVE=ocamlopt

LEX=ocamllex
YACC=ocamlyacc

# Makefile fragment containing auto-generated dependencies
DEPENDENCIES_FILE=Makefile.dependencies

# Mattieu Guillaumin's Minimalist Grammar to Multiple Context-free Grammar translator
# NB: Guillaumin's code should be patched like this and then recompiled:
#     patch ../guillaumin/hmg2mcfg/hmgtransform.ml hmgtransform-fixity.patch
GUILLAUMIN=../guillaumin/hmg2mcfg/hmg2mcfg

# Default to the grammars directory inside this directory.
# Any definition of GRAMMARS given on the command line will override this.
GRAMMARS=grammars

# Directory for auto-generated documentation
DOCDIR=doc

FLAGS= -I mcfgread -I +ocamlgraph
LIBS= nums.cmxa str.cmxa unix.cmxa graph.cmxa

# All source files that do not correspond to the "top" file of an executable.
MODULES= util.ml fsa.ml nelist.ml rule.ml chart.ml tables.ml parser.ml mcfgread/read.ml mcfgread/lexer.ml grammar.ml derivation.ml generate.ml path.ml

OCAMLINT= $(MODULES:.ml=.cmi)
OCAMLOBJ= $(MODULES:.ml=.cmx)

.PHONY: all
all: mcfg_nt train visualize cycles compare

mcfg_bc: $(OCAMLINT) $(OCAMLOBJ:.cmx=.cmo) main.cmo
	$(COMPILER_BYTECODE) $(FLAGS) -o $@ $(LIBS:.cmxa=.cma) $(OCAMLOBJ:.cmx=.cmo) main.cmo

mcfg_nt: $(OCAMLINT) $(OCAMLOBJ) main.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ $(LIBS) $(OCAMLOBJ) main.cmx

train: $(OCAMLINT) $(OCAMLOBJ) train.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ $(LIBS) $(OCAMLOBJ) train.cmx

visualize: $(OCAMLINT) $(OCAMLOBJ) visualize.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ $(LIBS) $(OCAMLOBJ) visualize.cmx

cycles: $(OCAMLINT) $(OCAMLOBJ) cycles.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ $(LIBS) $(OCAMLOBJ) cycles.cmx

compare: $(OCAMLINT) $(OCAMLOBJ) compare.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ $(LIBS) $(OCAMLOBJ) compare.cmx

# Dependencies
-include $(DEPENDENCIES_FILE)
$(DEPENDENCIES_FILE): *.mli *.ml mcfgread/lexer.ml mcfgread/read.ml mcfgread/read.mli
	ocamldep $(FLAGS) $^ > $@

.PHONY: clean
clean:
	rm -rf $(DOCDIR)
	rm -f $(DEPENDENCIES_FILE)
	rm -f *.o *.cmo *.cmi *.cmx
	rm -f mcfgread/*.o mcfgread/*.cmo mcfgread/*.cmi mcfgread/*.cmx mcfgread/lexer.ml mcfgread/read.ml mcfgread/read.mli
	rm -f mcfg_bc mcfg_nt train visualize cycles compare

# Stop make from deleting ``intermediate'' mcfg files.
# NB: (1) PRECIOUS is a lot like SECONDARY, but SECONDARY doesn't allow wildcards.
#     (2) The reason a file like, say, chart.cmx is not deleted is that it is not treated 
#         as ``intermediate'' because it is referred to explicitly in this Makefile.
.PRECIOUS: $(GRAMMARS)/mcfgs/%.mcfg

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

# If the previous rule didn't apply (i.e. if there is no .mli file), 
# then you can use this to produce a .cmi file.
%.cmi: %.ml
	$(COMPILER_BYTECODE) $(FLAGS) -c $*.ml

%.ml: %.mll
	$(LEX) $*.mll

%.mli: %.mly
	$(YACC) $*.mly

%.ml: %.mly
	$(YACC) $*.mly

# For generating documentation
.PHONY: doc
doc: *.mli $(OCAMLINT)
	mkdir -p $(DOCDIR)
	ocamldoc -html -d $(DOCDIR) $(FLAGS) *.mli

