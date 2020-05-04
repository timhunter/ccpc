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
GUILLAUMIN=../guillaumin/hmg2mcfg/hmg2mcfg

# Default to the grammars directory inside this directory.
# Any definition of GRAMMARS given on the command line will override this.
GRAMMARS=grammars

# Directory for auto-generated documentation
DOCDIR=doc

FLAGS= -I mcfgread -I +ocamlgraph
LIBS= nums.cmxa str.cmxa unix.cmxa graph.cmxa

# All source files that do not correspond to the "top" file of an executable.
MODULES= util.ml fsa.ml nelist.ml rule.ml chart.ml parser.ml mcfgread/read.ml mcfgread/lexer.ml matrix.ml grammar.ml derivation.ml path.ml

OCAMLINT= $(MODULES:.ml=.cmi)
OCAMLOBJ= $(MODULES:.ml=.cmx)

.PHONY: all
all: parse intersect train visualize cycles compare renormalize findentropy ccpctop

###########################################################################################
### These two executables (and the corresponding main.ml file) are now deprecated. Leaving 
### them here for a little bit so that they can still be built if necessary, but I'm 
### removing them from the list of executables that are made as part of the 'all' target. 
### One day soonish we should (a) delete this section of the Makefile, and (b) delete the 
### main.ml file.
### TH 2014-09-16

mcfg_bc: $(OCAMLINT) $(OCAMLOBJ:.cmx=.cmo) main.cmo
	$(COMPILER_BYTECODE) $(FLAGS) -o $@ $(LIBS:.cmxa=.cma) $(OCAMLOBJ:.cmx=.cmo) main.cmo

mcfg_nt: $(OCAMLINT) $(OCAMLOBJ) main.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ $(LIBS) $(OCAMLOBJ) main.cmx

###########################################################################################

# The weird '-warn-error -a' prevents Warning 31 from being treated as a fatal error.
# Not sure why it's required here and not for generating other executables.
# https://stackoverflow.com/questions/37415476/ocaml-warning-31-compiler-libs-and-ppx
ccpctop: $(OCAMLINT) $(MODULES:.ml=.cmo)
	ocamlmktop -warn-error -a -o ccpctop $(FLAGS) $(LIBS:.cmxa=.cma) $(MODULES:.ml=.cmo)

parse: $(OCAMLINT) $(OCAMLOBJ) parse.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ $(LIBS) $(OCAMLOBJ) parse.cmx

intersect: $(OCAMLINT) $(OCAMLOBJ) intersect.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ $(LIBS) $(OCAMLOBJ) intersect.cmx

train: $(OCAMLINT) $(OCAMLOBJ) train.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ $(LIBS) $(OCAMLOBJ) train.cmx

visualize: $(OCAMLINT) $(OCAMLOBJ) visualize.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ $(LIBS) $(OCAMLOBJ) visualize.cmx

cycles: $(OCAMLINT) $(OCAMLOBJ) cycles.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ $(LIBS) $(OCAMLOBJ) cycles.cmx

compare: $(OCAMLINT) $(OCAMLOBJ) compare.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ $(LIBS) $(OCAMLOBJ) compare.cmx

prefilter: $(OCAMLINT) $(OCAMLOBJ) prefilter.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ $(LIBS) $(OCAMLOBJ) prefilter.cmx

renormalize: $(OCAMLINT) $(OCAMLOBJ) renormalize.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ $(LIBS) $(OCAMLOBJ) renormalize.cmx

findentropy: $(OCAMLINT) $(OCAMLOBJ) findentropy.cmx
	$(COMPILER_NATIVE) $(FLAGS) -o $@ $(LIBS) $(OCAMLOBJ) findentropy.cmx


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
	rm -f mcfg_bc mcfg_nt parse intersect train visualize cycles compare renormalize findentropy

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

