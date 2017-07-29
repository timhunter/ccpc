# This program

PROG = simplex

# Setup

LIBS = \
	nums.cma

CAMLC = ocamlc
CAMLDOC = ocamldoc
CAMLFLAGS = -g

%.cmo: %.ml
	$(CAMLC) $(CAMLFLAGS) -c $<

# Source and Object files
SOURCES = \
	Order.ml EltsI.ml Elts.ml \
	Helpers.ml MatrixI.ml Matrix.ml \
	SimplexI.ml Interface.ml Main.ml
	
OBJECTS = $(SOURCES:.ml=.cmo)

# Final Program

$(PROG): $(OBJECTS)
	$(CAMLC) $(CAMLFLAGS) $(LIBS) $(OBJECTS) -o $(PROG)

# DocGen

doc: $(OBJECTS)
	$(CAMLDOC) -html $(SOURCES)

# Other

all: $(PROG)

clean:
	rm -rf *.cmo *.cmi *.html *.css $(PROG)

.DEFAULT_GOAL := $(PROG)
.PHONY: doc build run clean
