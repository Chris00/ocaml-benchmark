# $Id: Makefile,v 1.1.1.1 2004-08-18 21:29:29 chris_77 Exp $

PKGNAME	   = $(shell grep name META | sed -e "s/.*\"\([^\"]*\)\".*/\1/")
PKGVERSION = $(shell grep version META | sed -e "s/.*\"\([^\"]*\)\".*/\1/")

BYTE_OBJS    :=
OPT_OBJS     :=
REQUIRES     := 
PREDICATES   :=

OCAMLC       := ocamlfind ocamlc
OCAMLOPT     := ocamlfind ocamlopt
OCAMLDEP     := ocamlfind ocamldep
OCAMLDOC     := ocamldoc

DISTFILES    := INSTALL LICENSE META Makefile README \
		$(wildcard *.ml) $(wildcard *.mli) $(wildcard examples/)

default: all


######################################################################

C_FILES   := $(wildcard *.c)
ML_FILES  := $(wildcard *.ml)
MLI_FILES := $(wildcard *.mli)
CMI_FILES := $(addsuffix .cmi,$(basename $(MLI_FILES)))

C_OBJS    := $(if $(C_FILES),$(PKGNAME).o $(C_OBJS),)
BYTE_OBJS := $(if $(ML_FILES),$(PKGNAME).cmo $(BYTE_OBJS),)
OPT_OBJS  := $(if $(ML_FILES),$(PKGNAME).cmx $(OPT_OBJS),)

DOCFILES  += $(ML_FILES) $(MLI_FILES)
PUBFILES  += $(DOCFILES) README


ARCHIVE   := $(if $(ML_FILES),$(PKGNAME).cma,)
XARCHIVE  := $(if $(ML_FILES),$(PKGNAME).cmxa,)

PKGS := $(shell grep 'requires' META|sed 's/requires[ 	]*=[ 	]*//')
PACKAGE_OPTS    := $(if $(PKGS),-package $(PKGS),)
SYNTAX_OPTS     := $(if $(SYNTAX),-syntax $(SYNTAX),)
PP_OPTS         := $(if $(PPOPT),-ppopt $(PPOPT),)
PREDICATE_OPTS  := $(if $(PREDICATES),-predicates $(PREDICATES),)
OCAML_STDLIB    := $(shell ocamlfind printconf stdlib)

export OCAMLPATH = ..

.PHONY: all opt byte mli
all: $(C_OBJS) $(CMI_FILES) byte opt
byte: $(ARCHIVE)
opt: $(XARCHIVE)
mli: $(PKGNAME).mli.tmp

$(ARCHIVE): $(BYTE_OBJS)
	$(OCAMLC) -a -o $@ $(PREDICATE_OPTS) $<

$(XARCHIVE): $(OPT_OBJS)
	$(OCAMLOPT) -a -o $@ $(PREDICATE_OPTS) $<

.SUFFIXES: .cmo .cmi .cmx .ml .mli

%.mli.tmp: %.ml
	$(OCAMLC) $(PACKAGE_OPTS) $(PREDICATE_OPTS) $(SYNTAX_OPTS) -c $< -i > $@
	@[ -s $@ ] || rm -f $@

%.cmo: %.ml
	$(OCAMLC) $(PACKAGE_OPTS) $(PREDICATE_OPTS) $(SYNTAX_OPTS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(PACKAGE_OPTS) $(PREDICATE_OPTS) $(SYNTAX_OPTS) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(PACKAGE_OPTS) $(PREDICATE_OPTS) $(SYNTAX_OPTS) -c $<

%.o: %.c
	$(CC) -c -I. -I$(OCAML_STDLIB)/caml $<


.depend: $(ML_FILES) $(MLI_FILES)
	$(OCAMLDEP) $(SYNTAX_OPTS) $(ML_FILES) $(MLI_FILES) > $@
include .depend

# (Un)installation
.PHONY: install uninstall
install: all
	ocamlfind remove $(PKGNAME); \
	[ -f "$(XARCHIVE)" ] && \
	extra="$(XARCHIVE) $(basename $(XARCHIVE)).a"; \
	ocamlfind install $(PKGNAME) $(MLI_FILES) $(CMI_FILES) $(ARCHIVE) META $$extra $(C_OBJS)

uninstall:
	ocamlfind remove $(PKGNAME)

# Make the examples
.PHONY: ex
examples:
	cd examples/; $(MAKE)

# Compile HTML documentation
doc: $(DOCFILES) $(CMI_FILES)
	@if [ -n "$(DOCFILES)" ] ; then \
	    if [ ! -x doc ] ; then mkdir doc ; fi ; \
	    $(OCAMLDOC) -v -d doc -html -colorize-code -I +contrib \
		$(ODOC_OPT) $(DOCFILES) ; \
	fi

# Make a tarball
.PHONY: dist
dist: $(DISTFILES)
	@ if [ -z "$(PKGNAME)" ]; then echo "PKGNAME not defined"; exit 1; fi
	@ if [ -z "$(PKGVERSION)" ]; then \
		echo "PKGVERSION not defined"; exit 1; fi
	mkdir $(PKGNAME)-$(PKGVERSION) ; \
	cp -r $(DISTFILES) $(PKGNAME)-$(PKGVERSION)/; \
	tar --exclude "CVS" --exclude ".cvsignore" --exclude "*~" \
	   --exclude "*.cm{i,x,o,xa}" --exclude "*.o" \
	  -jcvf $(PKGNAME)-$(PKGVERSION).tar.bz2 $(PKGNAME)-$(PKGVERSION); \
	rm -rf $(PKGNAME)-$(PKGVERSION)

# Release a Sourceforge tarball and publish the corresponding HTML doc 
# FIXME
.PHONY: publish
publish: doc dist
	@if [ -z "$(PKGNAME)" ]; then echo "PKGNAME variable not defined!"; \
		exit 1; fi ; \
	PDIR="$(HOME)/www/ocaml/$(PKGNAME)/" ; \
	mkdir -p $$PDIR/ && cp $(PUBFILES) $$PDIR/ ; \
	if [ -f "$(PKGNAME)-$$VER.tar.bz2" ] ; then \
	  cp $(PKGNAME)-$$VER.tar.bz2 $$PDIR/ ; \
	fi ; \
	echo "# PUBLISHED FILES IN $$PDIR" ; \
	if [ -f "$$PDIR/README" ] ; then mv $$PDIR/README $$PDIR/README.txt; fi ; \
	if [ -d doc ] ; then \
	    mkdir -p $$PDIR/documentation/ ; \
	    cp -r doc/* $$PDIR/documentation/ ; \
	fi


.PHONY: clean distclean
clean:
	rm -f *~ *.cmi *.cmo *.cmx *.cma *.cmxa *.a *.o *.tmp
	rm -f $(PKGNAME)-$(PKGVERSION).tar.bz2
	rm -rf doc/
	cd examples/; $(MAKE) clean

distclean: clean
	rm -f config.status config.cache config.log
