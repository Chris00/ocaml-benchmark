# $Id: Makefile,v 1.6 2004-09-02 20:08:01 chris_77 Exp $

PKGNAME	   = $(shell grep "name" META | sed -e "s/.*\"\([^\"]*\)\".*/\1/")
PKGVERSION = $(shell grep "version" META | sed -e "s/.*\"\([^\"]*\)\".*/\1/")

SRC_WEB    = web
SF_WEB     = /home/groups/o/oc/ocaml-benchmark/htdocs

BYTE_OBJS    :=
OPT_OBJS     :=
REQUIRES     := 
PREDICATES   :=

OCAMLC       := ocamlc
OCAMLOPT     := ocamlopt
OCAMLDEP     := ocamldep
OCAMLDOC     := ocamldoc
OCAMLFIND    := ocamlfind

DISTFILES    := INSTALL LICENSE META Makefile README \
		$(wildcard *.ml) $(wildcard *.mli) $(wildcard examples/)

PKG_TARBALL  = $(PKGNAME)-$(PKGVERSION).tar.bz2

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

PKGS = $(shell grep "requires" META | sed -e "s/.*\"\([^\"]*\)\".*/\1/")
PKGS_CMA 	= $(addsuffix .cma, $(PKGS))
OCAML_STDLIB 	= $(shell ocamlc -where)

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
	$(OCAMLC) $(PKGS_CMA) -c $< -i > $@
	@[ -s $@ ] || rm -f $@

%.cmo: %.ml
	$(OCAMLC) $(PKGS_CMA) -c $<

%.cmi: %.mli
	$(OCAMLC) $(PKGS_CMA) -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(PKGS_CMA:.cma=.cmxa) -c $<

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
.PHONY: ex examples
ex: examples
examples:
	cd examples/; $(MAKE)

# Compile HTML documentation
doc: $(DOCFILES) $(CMI_FILES)
	@if [ -n "$(DOCFILES)" ] ; then \
	    if [ ! -x doc ] ; then mkdir doc ; fi ; \
	    $(OCAMLDOC) -v -d doc -html -colorize-code -I +contrib \
		$(ODOC_OPT) $(DOCFILES) ; \
	fi

# Make.bat -- easy compilation on win32
Make.bat:
	$(MAKE) clean
#	Filter out all "make" messages
	$(MAKE) all | grep --invert-match "make" > $@

# Make a tarball
.PHONY: dist
dist: $(DISTFILES) Make.bat
	@ if [ -z "$(PKGNAME)" ]; then echo "PKGNAME not defined"; exit 1; fi
	@ if [ -z "$(PKGVERSION)" ]; then \
		echo "PKGVERSION not defined"; exit 1; fi
	mkdir $(PKGNAME)-$(PKGVERSION) ; \
	mv Make.bat $(PKGNAME)-$(PKGVERSION); \
	cp -r $(DISTFILES) $(PKGNAME)-$(PKGVERSION)/; \
	tar --exclude "CVS" --exclude ".cvsignore" --exclude "*~" \
	   --exclude "*.cm{i,x,o,xa}" --exclude "*.o" \
	  -jcvf $(PKG_TARBALL) $(PKGNAME)-$(PKGVERSION); \
	rm -rf $(PKGNAME)-$(PKGVERSION)

# Release a Sourceforge tarball and publish the HTML doc 
.PHONY: web upload
web: doc
	@ if [ -d doc ] ; then \
	  scp -r doc/ shell.sf.net:$(SF_WEB)/ \
	  && echo "*** Published documentation on SF" ; \
	fi
	@ if [ -d $(SRC_WEB)/ ] ; then \
	  scp $(SRC_WEB)/*.html $(SRC_WEB)/*.jpg LICENSE \
	    shell.sf.net:$(SF_WEB) \
	  && echo "*** Published web site ($(SRC_WEB)/) on SF" ; \
	fi

upload: dist
	@ if [ -z "$(PKG_TARBALL)" ]; then \
		echo "PKG_TARBALL not defined"; exit 1; fi
	echo -e "bin\ncd incoming\nput $(PKG_TARBALL)" \
	  | ncftp -p chris_77@users.sf.net upload.sourceforge.net \
	  && echo "*** Uploaded $(PKG_TARBALL) to SF"



.PHONY: clean distclean
clean:
	rm -f *~ *.cmi *.cmo *.cmx *.cma *.cmxa *.a *.o *.tmp
	rm -f Make.bat $(PKGNAME)-$(PKGVERSION).tar.bz2
	rm -rf doc/
	cd examples/; $(MAKE) clean

distclean: clean
	rm -f config.status config.cache config.log
