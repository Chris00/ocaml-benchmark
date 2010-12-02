# Makefile for benchmark examples
#
# Just type "make"!

# Do not require that the package be installed to try the examples
REQUIRES 	= "str unix pcre"
BENCHMARK	= -I .. benchmark.cma

EXAMPLES        := $(wildcard *.ml)
PACKAGE_OPTS    := $(if $(REQUIRES), -package $(REQUIRES), )
PREDICATE_OPTS  := $(if $(PREDICATES), -predicates $(PREDICATES), )
SYNTAX_OPTS     := $(if $(SYNTAX),   -syntax $(SYNTAX), )
PP_OPTS         := $(if $(PPOPT),    -ppopt $(PPOPT), )
ALL_OPTS	= $(PACKAGE_OPTS) $(PREDICATE_OPTS) $(SYNTAX_OPTS) $(PP_OPTS)

OCAMLC		= ocamlfind ocamlc
OCAMLOPT	= ocamlfind ocamlopt

.PHONY: all

all: byte opt
byte: $(EXAMPLES:.ml=.exe)
opt:  $(EXAMPLES:.ml=.com)

%.exe: %.ml ../benchmark.cma
	OCAMLPATH="../.." $(OCAMLC) -o $@ $(ALL_OPTS) -linkpkg $(BENCHMARK) $<

%.com: %.ml ../benchmark.cmxa
	OCAMLPATH="../.." $(OCAMLOPT) -o $@ $(ALL_OPTS) -linkpkg $(BENCHMARK:.cma=.cmxa) $<

######################################################################
.PHONY: clean clobber

clean:
	rm -f $(EXAMPLES:.ml=.exe) $(EXAMPLES:.ml=.com) *.o *.cm[iox] *~

clobber: clean
	rm -f $(EXAMPLES)
