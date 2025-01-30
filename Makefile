PKGVERSION = $(shell git describe --always)
PKGTARBALL = benchmark-$(PKGVERSION).tbz

DUNE_OPTS?=

all build byte native:
	dune build $(DUNE_OPTS) @install @tests @examples

install uninstall:
	dune $@

doc:
	sed -e 's/%%VERSION%%/$(PKGVERSION)/' benchmark.mli \
	  > _build/default/benchmark.mli
	dune build $(DUNE_OPTS) @doc
	@echo '.def { background: #f0f0f0; }' \
	  >> _build/default/_doc/_html/odoc.css

lint:
	opam lint benchmark.opam

format:
	dune fmt --auto-promote

clean:
	dune clean

WATCH?=@install
watch:
	dune build $(DUNE_OPTS) $(WATCH) -w


.PHONY: all build byte native install uninstall doc lint clean watch
