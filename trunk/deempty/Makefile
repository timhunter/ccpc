TARGET=deempty

COMPILER_BYTECODE=ocamlc
COMPILER_NATIVE=ocamlopt
LEX=ocamllex
YACC=ocamlyacc

FLAGS=
NATIVEFLAGS=$(FLAGS) -inline 32 -ccopt "-O3"

OCAMLINT=nullcomponenttable.cmi rule.cmi componentmap.cmi parse.cmi utilities.cmi deempty.cmi
OCAMLOBJ=nullcomponenttable.cmo rule.cmo componentmap.cmo parse.cmo lexer.cmo deempty.cmo utilities.cmo
NATIVEOBJ=nullcomponenttable.cmx rule.cmx componentmap.cmx parse.cmx lexer.cmx deempty.cmx utilities.cmx main.cmx

$(TARGET): $(OCAMLINT) $(OCAMLOBJ) main.cmo
	$(COMPILER_BYTECODE) $(FLAGS) -o $@ $(OCAMLOBJ) main.cmo

$(TARGET)_opt: $(OCAMLINT) $(NATIVEOBJ)
	$(COMPILER_NATIVE) $(NATIVEFLAGS) -o $@ $(NATIVEOBJ)

$(TARGET)_toplevel: $(OCAMLINT) $(OCAMLOBJ) toplevel.cmi toplevel.cmo
	ocamlmktop -o $(TARGET)_toplevel $(OCAMLOBJ) toplevel.cmo

all: $(TARGET) $(TARGET)_opt $(TARGET)_toplevel

opt: $(TARGET)_opt

toplevel: $(TARGET)_toplevel

clean:
	rm -f *.o *.cmo *.cmi *.cmx *.exe $(TARGET) $(TARGET)_opt $(TARGET)_toplevel

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
