(* File: benchmark.mli

   Modified in Aug. 2004 by Troestler Christophe
   Christophe.Troestler(at)umh.ac.be

   Copyright 2002-2003, Doug Bagley
   http://www.bagley.org/~doug/ocaml/
   Based on the Perl module Benchmark.pm by Jarkko Hietaniemi and Tim Bunce

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation, with the
   special exception on linking described in file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*)
(* 	$Id: benchmark.mli,v 1.4 2004-08-20 21:08:25 chris_77 Exp $	 *)
(** This module implements benchmarking functions for measuring the
  run-time of one or many functions using latency (multiple
  repetitions) or throughput (repeat until some time period has
  passed) tests.

  {b Examples:}
  Run the function [f] with input [5000] for [10] iterations and
  print the CPU times:

{[	Benchmark.latency1 10 f 5000]}

  Run the tests {i foo}, {i bar} and {i baz} for at least [8] seconds each,
  printing the results of each test, and then print a cross tabulation
  of the results:

{[	open Benchmark
	let res = throughputN 8 [("foo", f, 1000000);
	                         ("bar", f, 2000000);
	                         ("baz", f, 3000000); ] in
	print_newline();
	tabulate res	                                            ]}

  Time how long it takes to run one part of your code:

{[	let t0 = Benchmark.make 0 in
	  (* do something here *)
	let b = Benchmark.sub (Benchmark.make 0) t0 in
	print_endline "Benchmark results:";
	print_endline (Benchmark.to_string b)                       ]}
*)

type t = {
  wall : float;	  (** Wallclock time (in seconds) *)
  utime : float;  (** This process User CPU time (in seconds) *)
  stime : float;  (** This process System CPU time (in seconds) *)
  cutime : float; (** Child process User CPU time (in seconds) *)
  cstime : float; (** Child process System CPU time (in seconds) *)
  iters : int;	  (** Number of iterations *)
}
  (** The information returned by timing tests. *)

type style =
  | No_child	(** Do not print child CPU times *)
  | No_parent	(** Do not print parent CPU times *)
  | All		(** Print parent and child CPU times *)
  | Auto	(** Is [No_child] unless there is child CPU used *)
  | Nil		(** Print nothing *)
  (** Style of the output. *)

val make : int -> t
  (** [Benchmark.make n] create a new {!Benchmark.t} structure with
    current time values and [n] iterations. *)

val add : t -> t -> t
  (** [Benchmark.add b1 b2] add {!Benchmark.t} structure [b1] to [b2].*)

val sub : t -> t -> t
  (** [Benchmark.sub b1 b2] subtract {!Benchmark.t} structure [b2]
    from [b1]. *)

val to_string : ?style:style -> ?fwidth:int -> ?fdigits:int -> t -> string
  (** [Benchmark.to_string ?style ?fwidth ?fdigits b] converts the
    {!Benchmark.t} structure to a formatted string.

    @param style printing style (default: [Auto])
    @param fwidth number of chars reserved for the numbers (default: [5])
    @param fdigits number of fractional digits of the numbers (default: [2])
  *)

type samples = (string * t list) list
  (** Association list that links the names of the tests to the list
    of their timings. *)

val merge : samples -> samples -> samples
  (** [merge l1 l2] merges the two association lists of timings [l1]
    and [l2] into a single one, concatenating the timings for the same
    names of [l1] and [l2]. *)

val latency1 :
  ?min_count:int ->
  ?min_cpu:float ->
  ?name:string ->
  ?style:style ->
  ?fwidth:int ->
  ?fdigits:int ->
  ?repeat:int ->
  int -> ('a -> 'b) -> 'a -> samples
  (** [Benchmark.latency1 ?min_count ?min_cpu ?name ?style ?fwidth ?fdigits
    n f x] runs one function [f] with input [x] for [n] iterations, and
    returns the results, which are also printed unless style is [Nil].

    @param min_count (default: 4)
    @param min_cpu (default: 0.4)
    @param name (default: [""])
    @param style printing style (default: [Auto])
    @param fwidth number of chars reserved for the numbers (default: [5])
    @param fdigits number of fractional digits of the numbers (default: [2])
  *)

val latencyN :
  ?min_count:int ->
  ?min_cpu:float ->
  ?style:style ->
  ?fwidth:int ->
  ?fdigits:int ->
  ?repeat:int ->
  int -> (string * ('a -> 'b) * 'a) list -> samples
  (** [Benchmark.latencyN ?min_count ?min_cpu ?style ?fwidth ?fdigits
    n funs] runs each function in list [funs] for [n] iterations.  The
    list [funs] has the structure: [(name, f, x); (name, f, x); ...],
    where [name] is a string that is the name to label the test, [f]
    is the function to run, and [x] is its input.  If style is not
    [Nil], then the results are printed.  Returns the results list,
    which can be passed to {!Benchmark.tabulate} if you want a
    comparison table.

    @param min_count (default: 4)
    @param min_cpu (default: 0.4)
    @param style printing style (default: [Auto])
    @param fwidth number of chars reserved for the numbers (default: [5])
    @param fdigits number of fractional digits of the numbers (default: [2])
  *)

val throughput1 :
  ?min_count:int ->
  ?min_cpu:float ->
  ?name:string ->
  ?style:style ->
  ?fwidth:int ->
  ?fdigits:int ->
  ?repeat:int ->
  int -> ('a -> 'b) -> 'a -> samples
  (** [Benchmark.throughput1 ?min_count ?min_cpu ?name ?style ?fwidth
    ?fdigits n f x] runs one function [f] with input [x] for at least
    [n] seconds, and returns the result. If [?style] is not [Nil] then
    the results are also printed. *)

val throughputN :
  ?min_count:int ->
  ?min_cpu:float ->
  ?style:style ->
  ?fwidth:int ->
  ?fdigits:int ->
  ?repeat:int ->
  int -> (string * ('a -> 'b) * 'a) list -> samples
  (** [Benchmark.throughputN ?min_count ?min_cpu ?style ?fwidth
    ?fdigits n funs] runs each function in list [funs] for at least
    [n] seconds. The list [funs] has the structure: [(name, f, x);
    (name, f, x); ...], where [name] is a string that is the name to
    label the test, [f] is the function to run, and [x] is its input.
    If style is not Nil, then the results are printed. Returns the
    resulting list which can be passed to {!Benchmark.tabulate} if you
    want a comparison table. *)

val tabulate : ?no_parent:bool -> ?confidence:float -> samples -> unit
 (** [Benchmark.tablulate results] prints a comparison table for a
   list of [results] obtained by {!Benchmark.latencyN} or
   {!Benchmark.throughputN} with each function compared to all the
   others.  The table is of the type

{[	      Rate  name1 name2 ...   OR         s/iter  name1 name2 ...
	name1  #/s   --    r12             name1   #      --    r12
	name2  #/s   r21   --              name2   #      r21   --
	...                                ...                            ]}

   where name1, name2,... are the labels of the tests sorted from
   slowest to fastest and rij is (ri - rj) expressed in percents of rj
   where ri and rj are the rates of namei and namej respectively.

   @param no_parent if [true], only take in account the times of the
   children (default: [false])

   @param confidence (default: 0.95) *)


(**/**)

val latency : ?repeat:int -> int -> ('a -> 'b) -> 'a -> t list
  (** [Benchmark.latency ?repeat n f x] measures [repeat] times the
    time needed by the function [f] to run with input [x] for [n]
    iterations.  It returns the list of such measures.

    @param repeat number of measures (default: [1]) *)

val throughput : ?repeat:int -> float -> ('a -> 'b) -> 'a -> t list
  (** [Benchmark.throughput ?repeat tmax f x] measures [repeat] times
    the throughput of the function [f] with input [x] running for at
    least [tmax] seconds.  It returns the list of such measures.

    @param repeat number of measures (default: [1]) *)
