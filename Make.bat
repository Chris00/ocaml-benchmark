ocamlc unix.cma -c benchmark.mli
ocamlc -c benchmark.ml
ocamlc -a -o benchmark.cma benchmark.cmo
ocamlopt -c benchmark.ml
ocamlopt -a -o benchmark.cmxa benchmark.cmx
