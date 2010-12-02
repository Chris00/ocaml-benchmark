(* $Id: numbers.ml,v 1.1.1.1 2004-08-18 21:29:29 chris_77 Exp $ *)

open Printf
open Benchmark

(* Test the speed of addition for native ints (unboxed), and
   Int32/Int64 (which are both boxed).

   The output looks something like numbers.out
*)

let f_int n =
  let rec loop i sum =
    if i < n then loop (i + 1) (sum + 1) else sum in
  loop 0 0

let f_int32 n =
  let rec loop i sum =
    if i < n then loop (i + 1) (Int32.add sum Int32.one) else sum in
  Int32.to_int (loop 0 Int32.zero)

let f_int64 n =
  let rec loop i sum =
    if i < n then loop (i + 1) (Int64.add sum Int64.one) else sum in
  Int64.to_int (loop 0 Int64.zero)

let () =
  (* print out the results of the f_* functions to doublecheck that
     they work as we intend. *)
  printf "f_int   666 = %d\n" (f_int   666);
  printf "f_int32 666 = %d\n" (f_int32 666);
  printf "f_int64 666 = %d\n" (f_int64 666);
  print_newline ();

  (* let's exercise the *1 functions: *)
  let res = latency1 ~name:"int-1-lat" 1000 f_int 10000 in
  let res = throughput1 ~name:"int-1-thru" 5 f_int 10000 in
  print_newline ();

  (* now let's exercise the *N functions: *)
  let res = throughputN ~repeat:5 10
              [("int",   f_int,   10000);
               ("int32", f_int32, 10000);
               ("int64", f_int64, 10000); ] in
  print_newline ();
  tabulate res;

  print_newline ();
  let res = latencyN 2000 [("int",   f_int,   10000);
                           ("int32", f_int32, 10000);
                           ("int64", f_int64, 10000); ] in
  print_newline ();
  tabulate res
