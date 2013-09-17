PKGNAME	    = $(shell oasis query name)
PKGVERSION  = $(shell oasis query version)
PKG_TARBALL = $(PKGNAME)-$(PKGVERSION).tar.gz

DISTFILES  = INSTALL.txt LICENSE.txt META Makefile README.md \
  benchmark.mllib _tags _oasis API.odocl \
  $(wildcard *.ml) $(wildcard *.mli) $(wildcard examples/*.ml) \
  $(wildcard tests/*.ml)

WEB = ocaml-benchmark.forge.ocamlcore.org:/home/groups/ocaml-benchmark/htdocs/


.PHONY: all byte native configure doc install uninstall reinstall upload-doc

all byte native setup.log: configure
	ocaml setup.ml -build

configure: setup.data
setup.data: setup.ml
	ocaml $< -configure --enable-examples --enable-tests

setup.ml: _oasis
	oasis setup

doc install uninstall reinstall: setup.log
	ocaml setup.ml -$@

upload-doc: doc
	scp -C -p -r _build/API.docdir $(WEB)

# Make a tarball
.PHONY: dist tar
dist tar: $(DISTFILES)
	mkdir $(PKGNAME)-$(PKGVERSION) ; \
	cp -a --parents $(DISTFILES) $(PKGNAME)-$(PKGVERSION)/; \
	tar -zcvf $(PKG_TARBALL) $(PKGNAME)-$(PKGVERSION); \
	rm -rf $(PKGNAME)-$(PKGVERSION)

# Release a Sourceforge tarball and publish the HTML doc
.PHONY: web upload
web: upload-doc
	@ if [ -d web ] ; then \
	  $(SCP) web/*.html web/*.css web/*.jpg LICENSE.txt $(WEB) \
	  && echo "*** Published web site." ; \
	fi


.PHONY: clean distclean
clean::
	ocaml setup.ml -clean
	$(RM) $(PKG_TARBALL)

distclean:
	ocaml setup.ml -distclean
	$(RM) $(wildcard *.ba[0-9] *.bak *~ *.odocl)
