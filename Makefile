PKGVERSION = $(shell git describe --always)
PKGTARBALL = benchmark-$(PKGVERSION).tbz

all build byte native:
	dune build @install @tests @examples

install uninstall:
	dune $@

doc:
	sed -e 's/%%VERSION%%/$(PKGVERSION)/' benchmark.mli \
	  > _build/default/benchmark.mli
	dune build @doc
	@echo '.def { background: #f0f0f0; }' \
	  >> _build/default/_doc/_html/odoc.css

lint:
	opam lint benchmark.opam

clean:
	dune clean

.PHONY: all build byte native install uninstall doc lint clean
