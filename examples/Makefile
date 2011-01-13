# Makefile for benchmark examples
#
# Just type "make"!

# Do not require that the package be installed to try the examples
REQUIRES 	= str unix bigarray
BENCHMARK	= -I .. benchmark.cma

EXAMPLES        = $(wildcard *.ml)
PACKAGE_OPTS    = $(if $(REQUIRES), -package "$(REQUIRES)", )
PREDICATE_OPTS  = $(if $(PREDICATES), -predicates $(PREDICATES), )
ALL_OPTS	= -annot $(PACKAGE_OPTS) $(PREDICATE_OPTS)

OCAMLC		= ocamlfind ocamlc
OCAMLOPT	= ocamlfind ocamlopt

.PHONY: all

all: byte opt native
byte: $(EXAMPLES:.ml=.exe)
opt native:  $(EXAMPLES:.ml=.com)

regexps.exe regexps.com: REQUIRES += pcre

%.exe: %.ml ../benchmark.cma
	$(OCAMLC) -o $@ $(ALL_OPTS) -linkpkg $(BENCHMARK) $<

%.com: %.ml ../benchmark.cmxa
	$(OCAMLOPT) -o $@ $(ALL_OPTS) -linkpkg $(BENCHMARK:.cma=.cmxa) $<

######################################################################
.PHONY: clean

clean:
	$(RM) -f $(EXAMPLES:.ml=.exe) $(EXAMPLES:.ml=.com)
	$(RM) $(wildcard *.o *.cm[iox] *~ *.annot *.dat)

