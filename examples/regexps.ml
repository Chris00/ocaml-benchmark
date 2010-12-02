(* $Id: regexps.ml,v 1.2 2007-01-31 16:41:49 chris_77 Exp $ *)

open Printf
open Benchmark

(* Test the speed of standard regular expressions vs. Pcre using a
   simple regexp with captures.

   The output looks something like regexps.out
*)

(* Create a chunk of data to search.
   It's full of "near hits", strings of "012345678"
   with a string on the end we are searching for: "0123456789" *)
let bigdata =
  let size = 500000 in
  let buf = Buffer.create size in
  for i = 1 to size/10 - 1 do Buffer.add_string buf "012345678 " done;
  Buffer.add_string buf "0123456789";
  Buffer.contents buf

let pcre_re = Pcre.regexp "(012345678) (0123456789)"
let str_re  = Str.regexp  "\\(012345678\\) \\(0123456789\\)"

let pcre_match dat =
  let group = Pcre.extract ~rex:pcre_re dat in
  (group.(1), group.(2))

let str_match dat =
  let _pos = Str.search_forward str_re dat 0 in
  (Str.matched_group 1 dat, Str.matched_group 2 dat)

let () =
  (* Print out the results of the functions to doublecheck that they
     work as we intend. *)
  let (a, b) = pcre_match bigdata in printf "Pcre matches: %s %s\n" a b;
  let (a, b) = str_match  bigdata in printf "Str  matches: %s %s\n" a b;
  print_newline ();

  let res = throughputN ~repeat:5 5
    [("pcre match", pcre_match, bigdata);
     ("str match",  str_match,  bigdata)] in
  print_newline();
  tabulate res

(*   print_newline(); *)
(*   let res = latencyN ~repeat:5 100 *)
(*               [("pcre match", pcre_match, bigdata); *)
(*                ("str match",  str_match,  bigdata)] in *)
(*   print_newline(); *)
(*   tabulate res *)
