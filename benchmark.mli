(* File: benchmark.mli

   Copyright Aug. 2004-present by Troestler Christophe
   Christophe.Troestler(at)umons.ac.be

   Copyright 2002-2003, Doug Bagley
   http://www.bagley.org/~doug/ocaml/
   Based on the Perl module Benchmark.pm by Jarkko Hietaniemi and Tim Bunce

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 3 as published by the Free Software Foundation, with the
   special exception on linking described in file LICENSE.txt.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE.txt for more details.
*)

(** Benchmark running times of code.

    This module implements benchmarking functions for measuring the
    run-time of one or many functions using latency (multiple
    repetitions) or throughput (repeat until some time period has
    passed) tests.

    {b Examples:}
    Run the function [f] with input [5000] for [10] iterations and
    print the CPU times:
    {[
    Benchmark.latency1 10 f 5000                        ]}

    Run the tests [foo], [bar] and [baz] three times for at least [8]
    seconds each, printing the results of each test, and then print a
    cross tabulation of the results:
    {[
    open Benchmark
    let res = throughputN ~repeat:3 8 [("foo", foo, 1000000);
                                       ("bar", bar, 2000000);
                                       ("baz", baz, 3000000); ] in
    print_newline();
    tabulate res                                                    ]}

    Time how long it takes to some piece of code:
    {[
    let t0 = Benchmark.make 0L in
    (* do something here *)
    let b = Benchmark.sub (Benchmark.make 0L) t0 in
    print_endline "Benchmark results:";
    print_endline (Benchmark.to_string b)                       ]}
 *)


(** {2 Timing and samples structures} *)

(** The information returned by timing tests. *)
type t = {
  wall : float;   (** Wallclock time (in seconds) *)
  utime : float;  (** This process User CPU time (in seconds) *)
  stime : float;  (** This process System CPU time (in seconds) *)
  cutime : float; (** Child process User CPU time (in seconds) *)
  cstime : float; (** Child process System CPU time (in seconds) *)
  iters : Int64.t;  (** Number of iterations. *)
}

(** Style of the output. *)
type style =
  | No_child    (** Do not print child CPU times *)
  | No_parent   (** Do not print parent CPU times *)
  | All         (** Print parent and child CPU times *)
  | Auto        (** Same as [No_child] unless there is child CPU used *)
  | Nil         (** Print nothing *)

val make : Int64.t -> t
  (** [Benchmark.make n] create a new {!Benchmark.t} structure with
      current time values and [n] iterations.  Only the integer part of
      [n] is used, the fractional part is ignored. *)

val add : t -> t -> t
  (** [Benchmark.add b1 b2] add {!Benchmark.t} structure [b1] to [b2]. *)

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


(** {2 Timing functions} *)

val throughputN :
  ?min_count:Int64.t ->
  ?style:style ->
  ?fwidth:int ->
  ?fdigits:int ->
  ?repeat:int ->
  int -> (string * ('a -> 'b) * 'a) list -> samples
  (** [Benchmark.throughputN ?min_count ?style ?fwidth ?fdigits t
      funs] runs each function in list [funs] for at least [t > 0]
      seconds.  The list [funs] has the structure: [[(name1, f1, x1);
      (name2, f2, x2); ...]], where [name1] is the name to label the
      first test, [f1] is the function to run, and [x1] is its
      input,...  If [~style] is not [Nil], then the results are
      printed.  Returns the resulting list which can be passed to
      {!Benchmark.tabulate} if you want a comparison table.

      REMARK that [t] is the running time of the functions, not of the
      repetition loop.  Thus a very fast running function will need
      lots of repetitions to make a difference of [t] seconds to the
      empty loop.  In this case, the running time of the loop will
      dominate the whole process which can therefore take much longer
      than [t] seconds.  If you are only interested in the {i
      relative} times of fast functions and not in their real running
      times, we recommend you wrap each of them in a loop.

      @param min_count a warning will be printed if the number of runs is
                       less than [min_count].  This is a first defense against
                       meaningless results. (default: [4L])
      @param style   printing style (default: [Auto])
      @param fwidth  number of chars reserved for the numbers (default: [5])
      @param fdigits number of fractional digits of the numbers (default: [2])

      @param repeat  number of times each function running time is measured.
                     The default is [1] to be compatible with the former
                     version of this library but it is highly recommended
                     to set it to a higher number to enable confidence
                     statistics to be performed by {!Benchmark.tabulate}.
  *)

val throughput1 :
  ?min_count:Int64.t ->
  ?style:style ->
  ?fwidth:int ->
  ?fdigits:int ->
  ?repeat:int ->
  int -> ?name:string -> ('a -> 'b) -> 'a -> samples
  (** [Benchmark.throughput1 ?min_count ?style ?fwidth ?fdigits t ?name f x]
    runs one function [f] with input [x] for at least [t] seconds, and
    returns the result, which is also printed unless [~style] is
    [Nil].  See {!Benchmark.throughputN} for more information.  *)

val latencyN :
  ?min_cpu:float ->
  ?style:style ->
  ?fwidth:int ->
  ?fdigits:int ->
  ?repeat:int ->
  Int64.t -> (string * ('a -> 'b) * 'a) list -> samples
  (** [Benchmark.latencyN ?min_cpu ?style ?fwidth ?fdigits n funs]
      runs each function in list [funs] for [n] iterations.  [n] must be
      at least 4.  The list [funs] has the structure: [[(name1, f1, x1);
      (name2, f2, x2); ...]], where [name1] is the name to label the
      first test, [f1] is the function to run, and [x1] is its input,...
      If style is not [Nil], then the results are printed.  Returns the
      results list, which can be passed to {!Benchmark.tabulate} if you
      want to print a comparison table.

      @raise Invalid_argument if [n < 4L].
      @param min_cpu a warning will be printed if the total CPU time is
                     less than [min_cpu].  This is a first defense against
                     meaningless results  (default: [0.4]).
      @param style   printing style (default: [Auto]).
      @param fwidth  number of chars reserved for the numbers (default: [5]).
      @param fdigits number of fractional digits of the numbers (default: [2]).

      @param repeat  number of times each function running time is measured.
                     The default is [1] to be compatible with the former
                     version of this library but it is highly recommended
                     to set it to a higher number to enable confidence
                     statistics to be performed by {!Benchmark.tabulate}.
      *)

val latency1 :
  ?min_cpu:float ->
  ?style:style ->
  ?fwidth:int ->
  ?fdigits:int ->
  ?repeat:int ->
  Int64.t -> ?name:string -> ('a -> 'b) -> 'a -> samples
  (** [Benchmark.latency1 ?min_cpu ?style ?fwidth ?fdigits n ?name f x]
      runs the function [f] with input [x] for [n] iterations, and
      returns the results, which are also printed unless [~style] is
      [Nil].  See {!Benchmark.latencyN} for more information. *)


val tabulate : ?no_parent:bool -> ?confidence:float -> samples -> unit
  (** [Benchmark.tablulate results] prints a comparison table for a
    list of [results] obtained by {!Benchmark.latencyN} or
    {!Benchmark.throughputN} with each function compared to all the
    others.  The table is of the type

{[            Rate name1 name2 ...   OR          s/iter name1 name2 ...
        name1  #/s    --   r12             name1   #       --   r12
        name2  #/s   r21    --             name2   #      r21    --
        ...                                ...                            ]}

    where name1, name2,... are the labels of the tests sorted from
    slowest to fastest and rij says how much namei is faster (or
    slower if < 0) than namej (technically it is equal to (ri - rj)
    expressed in percents of rj where ri and rj are the rates of namei
    and namej respectively).

    If several results are associated to a given name, they are used
    to compute a Student's statistic to check whether the rates are
    significantly different.  If ri and rj are not believed to be
    different, rij will be printed between brackets.

    @param no_parent if [true], only take in account the times of the
    children (default: [false]).

    @param confidence is used to determine the confidence interval for
    the Student's test.  (default: [0.95]).  *)

