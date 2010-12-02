OCAMLCFLAGS = -annot
OCAMLOPTFLAGS = -annot
OCAMLDOCFLAGS=-html -stars -colorize-code -I +contrib

PKGNAME	   = $(shell grep "name" META | sed -e "s/.*\"\([^\"]*\)\".*/\1/")
PKGVERSION = $(shell grep "version" META | sed -e "s/.*\"\([^\"]*\)\".*/\1/")

include Makefile.conf

DISTFILES  = INSTALL LICENSE META Makefile Makefile.conf README Make.bat \
		$(wildcard *.ml) $(wildcard *.mli) $(wildcard examples/)

# Publish
SCP = scp -C
PKG_TARBALL  = ocaml-$(PKGNAME)-$(PKGVERSION).tar.gz
SRC_WEB    = web
WEB = ocaml-benchmark.forge.ocamlcore.org:/home/groups/ocaml-benchmark/htdocs/

default:
	@echo -n "This project now uses 'ocamlbuild' for the build phase.  "
	@echo -e "Please type\n\tocamlbuild all.otarget"
	@echo "(If you nonetheless want to use 'make' type 'make all'.)"


######################################################################

ML_FILES  := $(wildcard *.ml)
MLI_FILES := $(wildcard *.mli)
CMI_FILES := $(addsuffix .cmi,$(basename $(MLI_FILES)))

BYTE_OBJS := $(if $(ML_FILES),$(PKGNAME).cmo $(BYTE_OBJS),)
OPT_OBJS  := $(if $(ML_FILES),$(PKGNAME).cmx $(OPT_OBJS),)

DOCFILES  += $(MLI_FILES)
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
examples: all
	cd examples/; $(MAKE)

# Compile HTML documentation
doc: $(DOCFILES) $(CMI_FILES)
	@if [ -n "$(DOCFILES)" ] ; then \
	    mkdir -p doc; \
	    $(OCAMLDOC) -v -d doc $(OCAMLDOCFLAGS) $(ODOC_OPT) $(DOCFILES) ; \
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
	cp -r $(DISTFILES) $(PKGNAME)-$(PKGVERSION)/; \
	tar --exclude "CVS" --exclude ".cvsignore" --exclude "*~" \
	  -zcvf $(PKG_TARBALL) $(PKGNAME)-$(PKGVERSION); \
	rm -rf $(PKGNAME)-$(PKGVERSION)

# Release a Sourceforge tarball and publish the HTML doc
.PHONY: web upload
web: doc
	@ if [ -d doc ] ; then \
	  $(SCP) -r doc/ $(WEB)/ \
	  && echo "*** Published documentation" ; \
	fi
	@ if [ -d $(SRC_WEB)/ ] ; then \
	  $(SCP) $(SRC_WEB)/*.html $(SRC_WEB)/*.jpg LICENSE $(WEB) \
	  && echo "*** Published web site ($(SRC_WEB)/)" ; \
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
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

%.cmo: %.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

%.cma: %.cmo
	$(OCAMLC) -a -o $@ $(OCAMLCFLAGS) $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

%.cmxa: %.cmx
	$(OCAMLOPT) -a -o $@ $(OCAMLOPTFLAGS) $<


.PHONY: depend
depend: .depend
.depend: $(ML_FILES) $(MLI_FILES)
	$(OCAMLDEP) $(SYNTAX_OPTS) $(ML_FILES) $(MLI_FILES) > $@
include .depend

######################################################################
.PHONY: clean distclean
clean:
	rm -f *~ *.cmi *.cmo *.cmx *.cma *.cmxa *.a *.o *.tmp
	rm -f Make.bat $(PKG_TARBALL) *.dat
	rm -rf doc/
	cd examples/; $(MAKE) clean

distclean: clean
	rm -f config.status config.cache config.log .depend
