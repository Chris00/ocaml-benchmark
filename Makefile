# $Id: Makefile,v 1.9 2006-07-08 09:05:32 chris_77 Exp $

PKGNAME	   = $(shell grep "name" META | sed -e "s/.*\"\([^\"]*\)\".*/\1/")
PKGVERSION = $(shell grep "version" META | sed -e "s/.*\"\([^\"]*\)\".*/\1/")

include Makefile.conf

DISTFILES  = INSTALL LICENSE META Makefile README Make.bat \
		$(wildcard *.ml) $(wildcard *.mli) $(wildcard examples/)

# Publish
PKG_TARBALL  = ocaml-$(PKGNAME)-$(PKGVERSION).tar.bz2
SRC_WEB    = web
SF_WEB     = /home/groups/o/oc/ocaml-benchmark/htdocs

default: all


######################################################################

ML_FILES  := $(wildcard *.ml)
MLI_FILES := $(wildcard *.mli)
CMI_FILES := $(addsuffix .cmi,$(basename $(MLI_FILES)))

BYTE_OBJS := $(if $(ML_FILES),$(PKGNAME).cmo $(BYTE_OBJS),)
OPT_OBJS  := $(if $(ML_FILES),$(PKGNAME).cmx $(OPT_OBJS),)

DOCFILES  += $(ML_FILES) $(MLI_FILES)
PUBFILES  += $(DOCFILES) README

ARCHIVE   := $(if $(ML_FILES),$(PKGNAME).cma,)
XARCHIVE  := $(if $(ML_FILES),$(PKGNAME).cmxa,)

PKGS = $(shell grep "requires" META | sed -e "s/.*\"\([^\"]*\)\".*/\1/")
PKGS_CMA  = $(addsuffix .cma, $(PKGS))

export OCAMLPATH = ..

.PHONY: all opt byte
all: $(CMI_FILES) byte opt
byte: $(ARCHIVE)
opt: $(XARCHIVE)

# (Un)installation
.PHONY: install uninstall
install: all
	$(OCAMLFIND) remove $(PKGNAME); \
	[ -f "$(XARCHIVE)" ] && \
	extra="$(XARCHIVE) $(basename $(XARCHIVE)).a"; \
	$(OCAMLFIND) install $(if $(DESTDIR),-destdir $(DESTDIR)) $(PKGNAME) \
	$(MLI_FILES) $(CMI_FILES) $(ARCHIVE) META $$extra

installbyte:
	$(OCAMLFIND) remove $(PKGNAME); \
	$(OCAMLFIND) install $(if $(DESTDIR),-destdir $(DESTDIR)) $(PKGNAME) \
	$(MLI_FILES) $(CMI_FILES) $(ARCHIVE) META


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


# Caml general dependencies
.SUFFIXES: .cmo .cmi .cmx .ml .mli

%.cmi: %.mli
	$(OCAMLC) $(PKGS_CMA) -c $<

%.cmo: %.ml
	$(OCAMLC) -c $<

%.cma: %.cmo
	$(OCAMLC) -a -o $@ $<

%.cmx: %.ml
	$(OCAMLOPT) -c $<

%.cmxa: %.cmx
	$(OCAMLOPT) -a -o $@ $<


.PHONY: depend
depend: .depend.ocaml
.depend.ocaml: $(ML_FILES) $(MLI_FILES)
	$(OCAMLDEP) $(SYNTAX_OPTS) $(ML_FILES) $(MLI_FILES) > $@
include .depend.ocaml

######################################################################
.PHONY: clean distclean
clean:
	rm -f *~ *.cmi *.cmo *.cmx *.cma *.cmxa *.a *.o *.tmp
	rm -f Make.bat $(PKGNAME)-$(PKGVERSION).tar.bz2
	rm -rf doc/
	cd examples/; $(MAKE) clean

distclean: clean
	rm -f config.status config.cache config.log
