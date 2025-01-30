(lang dune 2.0)
(generate_opam_files true)

(name benchmark)
(version 1.7)
(homepage "https://github.com/Chris00/ocaml-benchmark")
(source (github Chris00/ocaml-benchmark))
(license "LGPL-3.0 with OCaml linking exception")
(maintainers "Christophe Troestler <Christophe.Troestler@umons.ac.be>")
(authors "Christophe Troestler <Christophe.Troestler@umons.ac.be>"
          "Doug Bagley" "c-cube")

(package
  (name benchmark)
  (synopsis "Benchmark running times of code")
  (description "This module provides a set of tools to measure the running times of
your functions and to easily compare the results.  A statistical test
is used to determine whether the results truly differ.")
  (depends
    (ocaml (>= 4.03))
    base-unix
    (dune (>= 2.0))))
