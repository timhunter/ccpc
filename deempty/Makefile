TARGET=deempty

COMPILER_BYTECODE=ocamlc
COMPILER_NATIVE=ocamlopt
LEX=ocamllex
YACC=ocamlyacc

FLAGS=
NATIVEFLAGS=$(FLAGS) -inline 32 -ccopt "-O3"

OCAMLINT=nullcomponenttable.cmi rule.cmi componentmap.cmi parse.cmi utilities.cmi deempty.cmi
OCAMLOBJ=nullcomponenttable.cmo rule.cmo componentmap.cmo parse.cmo lexer.cmo deempty.cmo utilities.cmo main.cmo
NATIVEOBJ=nullcomponenttable.cmx rule.cmx componentmap.cmx parse.cmx lexer.cmx deempty.cmx utilities.cmx main.cmx

$(TARGET): $(OCAMLINT) $(OCAMLOBJ)
	$(COMPILER_BYTECODE) $(FLAGS) -o $@ $(OCAMLOBJ)

$(TARGET)_opt: $(OCAMLINT) $(NATIVEOBJ)
	$(COMPILER_NATIVE) $(NATIVEFLAGS) -o $@ $(NATIVEOBJ)

all: $(TARGET) $(TARGET)_opt

opt: $(TARGET)_opt

clean:
	rm -f *.o *.cmo *.cmi *.cmx *.exe $(TARGET) $(TARGE)_opt

%.cmi: %.mli
	$(COMPILER_BYTECODE) $(FLAGS) -c $*.mli

%.cmx: %.ml
	$(COMPILER_NATIVE) $(NATIVEFLAGS) -c $*.ml

%.cmo: %.ml
	$(COMPILER_BYTECODE) $(FLAGS) -c $*.ml


%.ml: %.mll
	$(LEX) $*.mll

%.mli: %.mly
	$(YACC) $*.mly

%.ml: %.mly
	$(YACC) $*.mly
