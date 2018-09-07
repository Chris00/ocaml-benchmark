(* File: benchmark.ml
   For comparing runtime of functions
   *********************************************************************

   Copyright 2004-present, Troestler Christophe
   Christophe.Troestler(at)umh.ac.be

   Copyright 2002-2003, Doug Bagley
   http://www.bagley.org/~doug/ocaml/
   Initially based on the Perl module Benchmark.pm by Jarkko Hietaniemi
   and Tim Bunce

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 3 as published by the Free Software Foundation, with the
   special exception on linking described in file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE.txt for more details.
*)

open Printf

type t = {
  wall   : float;
  utime  : float;
  stime  : float;
  cutime : float;
  cstime : float;
  iters  : Int64.t;
  (* As of version 0.8, one had to change [iter] from [int] because,
     as machines run faster, a number of iterations ~ 2^29 is no
     longer enough (2^29 is the largest > 0 power of 2 that [int] can
     hold on a 32 bits platform. *)
}

type style = No_child | No_parent | All | Auto | Nil

let null_t =
  { wall = 0.; utime = 0.; stime = 0.; cutime = 0.; cstime = 0.; iters = 0L }

let make n =
  let tms = Unix.times() in
  { wall = Unix.gettimeofday();
    utime = tms.Unix.tms_utime;   stime = tms.Unix.tms_stime;
    cutime = tms.Unix.tms_cutime; cstime = tms.Unix.tms_cstime;
    iters = n }

let add a b =
  { wall = a.wall +. b.wall;       utime = a.utime +. b.utime;
    stime = a.stime +. b.stime;    cutime = a.cutime +. b.cutime;
    cstime = a.cstime +. b.cstime; iters = Int64.add a.iters b.iters }

let sub a b =
  { wall = a.wall -. b.wall;       utime = a.utime -. b.utime;
    stime = a.stime -. b.stime;    cutime = a.cutime -. b.cutime;
    cstime = a.cstime -. b.cstime; iters = Int64.sub a.iters b.iters }

(* It may happen that, because of slight variations, the running time
   of a fast running test is less than the running time of the null
   loop.  Returning a negative result is obviously ridiculous, thus
   one returns 0. *)
let ( -- ) a b = if (a:float) > b then a -. b else 0.
let pos_sub a b =
  { wall = a.wall -- b.wall;       utime = a.utime -- b.utime;
    stime = a.stime -- b.stime;    cutime = a.cutime -- b.cutime;
    cstime = a.cstime -- b.cstime; iters = Int64.sub a.iters b.iters }


let cpu_process b = b.utime +. b.stime
let cpu_childs b = b.cutime +. b.cstime
let cpu_all b = b.utime +. b.stime +. b.cutime +. b.cstime

(* Return a formatted representation of benchmark structure according
   to [style].  Default values for presentation parameters are set
   here. *)
let to_string ?(style=Auto) ?(fwidth=5) ?(fdigits=2) b =
  let pt = cpu_process b
  and ct = cpu_childs b in
  let style =
    if style = Auto then if ct > 1e-10 then All else No_child else style in
  let iter_info t =
    if b.iters > 0L && t > 0.0 then
      sprintf " @ %*.*f/s (n=%Ld)" fwidth fdigits
        (Int64.to_float b.iters /. t) b.iters
    else ""  in
  let f x = sprintf "%*.*f" fwidth fdigits x in
  match style with
  | All ->
      sprintf "%s WALL (%s usr %s sys + %s cusr %s csys = %s CPU)%s"
        (f b.wall) (f b.utime) (f b.stime) (f b.cutime) (f b.cstime)
        (f(pt +. ct)) (iter_info pt)
  | No_child ->
      sprintf "%s WALL (%s usr + %s sys = %s CPU)%s"
        (f b.wall) (f b.utime) (f b.stime) (f pt) (iter_info pt)
  | No_parent ->
      sprintf "%s WALL (%s cusr + %s csys = %s CPU)%s"
        (f b.wall) (f b.cutime) (f b.cstime) (f ct) (iter_info ct)
  | Nil -> ""
  | Auto -> assert false


(* Returns a string in minutes-seconds of a time [t >= 0] given in
   seconds. *)
let rec string_of_time t =
  if t = 0 || t = 1 then string_of_int t ^ "s"
  else if t < 60 then string_of_int t ^ "s"
  else if t < 120 then "1m " ^ string_of_time(t - 60)
  else string_of_int(t / 60) ^ "m " ^ string_of_time(t mod 60)

(* The time [t >= 0] is rounded to the nearest integer: *)
let string_of_time t = string_of_time(truncate(t +. 0.5))


type samples = (string * t list) list

let by_name (s1, _) (s2, _) = compare (s1:string) s2

let merge (l1:samples) (l2:samples) =
  (* [do_merge] assumes [l1] and [l2] are sorted. *)
  let rec do_merge acc l1 l2 =
    match l1, l2 with
    | _, [] -> acc @ l1
    | [], _ -> acc @ l2
    | ((n1, t1) as d1) :: tl1, ((n2, t2) as d2) :: tl2 ->
        let sgn = compare n1 n2 in
        if sgn = 0 then do_merge ((n1, t1 @ t2) :: acc) tl1 tl2
        else if sgn < 0 then do_merge (d1 :: acc) tl1 l2
        else do_merge (d2 :: acc) l1 tl2 in
  do_merge [] (List.sort by_name l1) (List.sort by_name l2)


let max_iter = Int64.add (Int64.of_int max_int) 1L
  (* even if [int] is 63 bits, [(max_iter:Int64.t) > 0] *)

(* [runloop n_iters n f x] returns the elapsed time of running [n >=
   0L] times [f] with the argument [x].  The structure returned
   declare [n_iter] iterations. *)
let runloop n_iters n f x =
  let n' = Int64.div n max_iter in
  if n' >= max_iter then
    invalid_arg "Benchmark.runloop: number of iterations too large";
  let n1 = Int64.to_int n'
  and n0 = Int64.to_int(Int64.rem n max_iter) in
  let t0 = ref (make 0L) in
  let tbase = !t0.utime in
  (* Wait for user timer to tick.  This makes the error range more
     like -0.01, +0.  If we don't wait, then it's more like -0.01,
     +0.01. *)
  while tbase = (!t0).utime do t0 := make 0L done;
  (* Loop over function we are timing [n] times (looping on int64
     quantities takes too long, this is why we use composite loops). *)
  for _ = 1 to n1 do
    for _ = 0 to max_int do ignore(f x) done; (* [max_iter] runs *)
  done;
  for _ = 1 to n0 do ignore(f x) done;
  let t1 = make n_iters in
  pos_sub t1 !t0

(* time a null-loop; no iter count *)
let null_loop n = runloop 0L n ignore ()

(* Run function [f] count times, return time taken (all times
   garanteed to be [>= 0.]) *)
let timeit n f x =
  let bn = null_loop n in
  let bm = runloop n n f x in
  pos_sub bm bn (* time of function minus null-loop *),
  bn.wall +. bm.wall (* how much the used had to wait *)


type printer = {
  print_indent : string -> unit; (* prefix, flushes *)
  print : string -> unit; (* No prefix but flushes *)
}

(* [print_run ff bm] prints the list of timings [bm] according to the
   style defined by the optional parameters. *)
let print_run out ?(min_count=4L) ?(min_cpu=0.4) ~style ?fwidth ?fdigits b =
  out.print_indent(to_string ~style ?fwidth ?fdigits b ^ "\n");
  if b.iters < min_count || cpu_all b < min_cpu
     || (b.wall < 1. && b.iters < 1000L)
  then out.print_indent "(warning: too few iterations for a reliable count)\n"


let latency n out ?min_count ?min_cpu ~style ?fwidth ?fdigits
    ~repeat _name f x =
  let rec loop nrep acc =
    if nrep < 1 then acc
    else (
      Gc.compact(); (* Reclaim memory to avoid undue GC during the test. *)
      let bm, _ = timeit n f x in
      print_run out ?min_count ?min_cpu ~style ?fwidth ?fdigits bm;
      loop (nrep - 1) (bm :: acc)
    ) in
  loop repeat []


(* Read the code from bottom to top: [min_iter] determines the minimal
   number of iterations to have a significant timing, then
   [estimate_niter] estimate by linear interpolation the number of
   iter to run [> tmin] and then the test is performed. *)
let throughput tmin out ?min_count ?min_cpu ~style ?fwidth ?fdigits
    ~repeat _name f x =
  (* Run [f] for [niter] times and complete with >= [nmin] iterations
     (estimated by linear interpolation) to run >= [tmin]. *)
  let rec run_test nmin niter bm_init total_wall =
    let bm, wall = timeit niter f x in
    let bm = add bm_init bm in
    let tn = cpu_process bm in
    let total_wall = total_wall +. wall in
    if tn >= tmin then (
      print_run out ?min_count ?min_cpu ~style ?fwidth ?fdigits bm;
      bm, total_wall
    )
    else
      (* FIXME *)
      let n = Int64.of_float((tmin /. tn -. 1.) *. Int64.to_float bm.iters) in
      run_test nmin (max nmin n) bm total_wall in
  (* Repeat the test [nrep] times and return the list of results. *)
  let rec repeat_test nrep acc nmin niter wall_estim =
    if nrep < 1 then acc else (
      Gc.compact(); (* Reclaim memory to avoid undue GC during the test. *)
      let bm, wall = run_test nmin niter null_t 0. in
      let wall_estim =
        if wall > wall_estim +. 60. then (
          out.print_indent("(Estimated time for subsequent runs: "
                            ^ (string_of_time wall) ^ ")\n");
          wall
        )
        else wall_estim in
      repeat_test (nrep - 1) (bm :: acc) nmin niter wall_estim
    ) in
  (* Estimate number of iter > [nmin] to have a running time >=
     [tmin].  The initial estimate is [n] running [tn] secs.  Linear
     estimates bear a 5% fudge to improve the overall responsiveness. *)
  let tpra = 0.1 *. tmin (* Target/time practice *) in
  let rec estimate_niter nmin n tn wall =
    if tn >= tpra then
      (* FIXME: *)
      (* lin estim *)
      let niter = Int64.of_float(Int64.to_float n *. (1.05 *. tmin /. tn)) in
      let wall_estim = wall *. (1.05 *. tmin /. tn) in
      if wall_estim >= 60. then
        out.print_indent("(Estimated time for each run: "
                         ^ (string_of_time wall_estim) ^ ")\n");
      repeat_test repeat [] nmin (max nmin niter) wall_estim
    else
      (* lin estim *)
      let new_n = Int64.of_float(Int64.to_float n *. 1.05 *. tpra /. tn) in
      let new_bn, new_wall = timeit new_n f x in
      let new_tn = cpu_process new_bn in
      let n = (* make sure we make progress *)
        if new_tn > 1.2 *. tn then new_n
        else Int64.of_float(1.1 *. Int64.to_float n +. 1.) (* FIXME *) in
      estimate_niter nmin n new_tn new_wall in
  (* Determine the minimum number of iterations to run >= 0.1 sec
     (whatever [tmin]).  Inform the user if it takes too long. *)
  let rec min_iter n ~takes_long:previous_took_long total_wall =
    if n <= 0L then
      failwith "throughput: number of iterations too large for Int64.t storage";
    let bm, wall = timeit n f x in
    let tn = cpu_process bm in
    let total_wall = total_wall +. wall in
    if tn < 0.1 then (
      let takes_long = total_wall >= 30. in
      if takes_long then (
        if total_wall >= 120. then (
          out.print " canceled)\n";
          failwith(sprintf "Benchmark.throughputN: wall time is %g while \
            CPU time is %g.  Do you use \"sleep\"?" total_wall tn)
        )
        else if previous_took_long then out.print "." else
          out.print_indent("(Determining how many runs to perform, \
            please be patient...");
        );
      let twice_n = Int64.shift_left n 1 in
      min_iter twice_n ~takes_long total_wall
    )
    else (
      if previous_took_long then out.print ")\n";
      if tn < tmin then estimate_niter n n tn wall (* tn > 0.1 *)
      else ( (* minimal [n] good for [tmin], use the above measurement
                for the first run. *)
        print_run out ?min_count ?min_cpu ~style ?fwidth ?fdigits bm;
        repeat_test (repeat - 1) [bm] n n wall
      )
    ) in
  min_iter 1L ~takes_long:false 0.


(* Make a print function that prefixes each output except the first
   one by [nspace] spaces. *)
let make_printer nspace =
  let first = ref true in
  let prefix = String.make nspace ' ' in
  let print s = print_string s; flush stdout in
  let print_indent s =
    if !first then first := false else print_string prefix;
    print s in
  { print_indent = print_indent; print = print; }

let null_printer = { print_indent = (fun _ -> ());  print = (fun _ -> ()) }


(* Generic interface for performing measurements on a list of functions *)
let testN ~test default_f_name  ?min_count ?min_cpu ~style
    ?fwidth ?fdigits ~repeat funs =
  let length_name =
    List.fold_left (fun m (n,_,_) -> max m (String.length n)) 0 funs in
  let result_of (name, f, x) =
    if style <> Nil then
      printf "%*s: %!" length_name (if name = "" then default_f_name else name);
    let out = if style = Nil then null_printer
              else make_printer (length_name + 2) in
    let bm = test out ?min_count ?min_cpu ~style ?fwidth ?fdigits
                  ~repeat name f x in
    (name, bm) in
  List.map result_of funs

let string_of_names funs =
  String.concat ", " (List.map (fun (a,_,_) -> sprintf "%S" a) funs)


let latencyN ?min_cpu ?(style=Auto) ?fwidth ?fdigits ?(repeat=1) n funs =
  if n < 4L then invalid_arg "Benchmark.latencyN: n < 4";
  if style <> Nil then (
    printf "Latencies for %Ld iterations of %s%s:\n%!" n
      (string_of_names funs)
      (if repeat > 1 then sprintf " (%i runs)" repeat else "");
  );
  testN ~test:(latency n) (sprintf "[run %Ld times]" n)
    ?min_cpu ~style ?fwidth ?fdigits ~repeat funs

let latency1 ?min_cpu ?style ?fwidth ?fdigits ?repeat  n ?(name="") f x =
  if n < 4L then invalid_arg "Benchmark.latency1";
  latencyN ?min_cpu ?style ?fwidth ?fdigits ?repeat n [(name, f, x)]


let throughputN ?min_count ?(style=Auto) ?fwidth ?fdigits ?(repeat=1) n funs =
  if n <= 0 then invalid_arg "Benchmark.throughputN: n <= 0";
  let tmin = float n in
  if style <> Nil then (
    printf "Throughputs for %s%s running%s for at least %g CPU second%s:\n%!"
      (string_of_names funs)
      (if List.length funs > 1 then " each" else "")
      (if repeat > 1 then sprintf " %i times" repeat else "")
      tmin (if n > 1 then "s" else "");
  );
  testN ~test:(throughput tmin) (sprintf "[run > %3.1g secs]" tmin)
    ?min_count ~style ?fwidth ?fdigits ~repeat funs

let throughput1 ?min_count ?style ?fwidth ?fdigits ?repeat n ?(name="") f x =
  if n <= 0 then invalid_arg "Benchmark.throughput1: n <= 0";
  throughputN ?min_count ?style ?fwidth ?fdigits ?repeat n [(name, f, x)]


(* Statistical tests and comparison table
 ***********************************************************************)

(* Utility functions *)
let list_mapi f =
  let rec loop i = function
    | [] -> []
    | a::l -> let r = f i a in r :: loop (i + 1) l in
  loop 0

let list_iteri f =
  let rec loop i = function
    | [] -> ()
    | a::l -> let () = f i a in loop (i + 1) l in
  loop 0

let is_nan x = (classify_float x = FP_nan)


(* [log_gamma x] computes the logarithm of the Gamma function at [x]
   using Lanczos method.  It is assumed [x > 0.].

   See e.g. http://home.att.net/~numericana/answer/info/godfrey.htm *)
let log_gamma =
  let c = [|    1.000000000000000174663;
             5716.400188274341379136;
           -14815.30426768413909044;
            14291.49277657478554025;
            -6348.160217641458813289;
             1301.608286058321874105;
             -108.1767053514369634679;
                2.605696505611755827729;
               -0.7423452510201416151527e-2;
                0.5384136432509564062961e-7;
               -0.4023533141268236372067e-8 |] in
  let c_last = Array.length c - 1 in
  let g = float(c_last - 1) in
  let sqrt2pi = sqrt(8. *. atan 1.) in
  let rec sum i den s =
    if i > 0 then sum (i - 1) (den -. 1.) (s +. c.(i) /. den)
    else c.(0) +. s in
  fun x ->
    assert(x > 0.);
    let xg = x +. g in
    let xg_5 = xg -. 0.5 in
    log(sqrt2pi *. sum c_last xg 0.) +. (x -. 0.5) *. log xg_5 -. xg_5

(* Beta function.  It is assumed [a > 0. && b > 0.]. *)
let _beta a b =
  assert(a > 0. && b > 0.);
  exp(log_gamma a +. log_gamma b -. log_gamma(a +. b))

(* [betai x a b] returns the value of the incomplete Beta function
   I_x(a,b).  It is evaluated through the continued fraction expansion
   (see e.g. Numerical Recipies, 6.4):

              x^a (1-x)^b [  1  d1  d2     ]
   I_x(a,b) = ----------- [ --  --  -- ... ]
               a  B(a,b)  [ 1+  1+  1+     ]

   where B(a,b) is the beta function and

               m (b-m) x                       - (a + m)(a + b + m) x
   d_2m = --------------------      d_(2m+1) = ----------------------
          (a + 2m - 1)(a + 2m)                  (a + 2m)(a + 2m + 1)

   The modified Lentz's method is used for the continued fraction (see
   NR, section 5.2) in routine [betai_cf].
*)
let max_tiny x = max 1e-30 x (* to avoid null divisors *)

let betai_cf_eps = epsilon_float

let betai_cf x a b =
  let apb = a +. b
  and ap1 = a +. 1.
  and am1 = a -. 1. in
  let rec lentz m c d f =
    let m2 = 2. *. m in
    (* Even rec step d_2m *)
    let cf_d2m = m *. (b -. m) *. x /. ((am1 +. m2) *. (a +. m2)) in
    let d = 1. /. max_tiny(1. +. cf_d2m *. d)
    and c = max_tiny(1. +. cf_d2m /. c) in
    let f = f *. d *. c in
    (* Odd rec step d_2m+1 *)
    let cf_d2m1 = -. (a +. m) *. (apb +. m) *. x
                  /. ((a +. m2) *. (ap1 +. m2)) in
    let d = 1. /. max_tiny(1. +. cf_d2m1 *. d)
    and c = max_tiny(1. +. cf_d2m1 /. c) in
    let delta = c *. d in
    let f = f *. delta in
    if abs_float(delta -. 1.) < betai_cf_eps then f
    else lentz (m +. 1.) c d f in
  (* Initialize Lentz's method with C2=1, D2 (step 2) *)
  let d2 = 1. /. max_tiny(1. -. apb *. x /. ap1) in
  lentz 1. 1. d2 d2

let betai x a b =
  assert(a > 0. && b > 0.);
  if x < 0. || x > 1. then invalid_arg "betai";
  if x = 0. then 0.
  else if x = 1. then 1.
  else
    let m = exp(log_gamma(a +. b) -. log_gamma a -. log_gamma b
                +. a *. log x +. b *. log(1. -. x)) in
    if x < (a +. 1.) /. (a +. b +. 2.)
    then m *. betai_cf x a b /. a
    else 1. -. m *. betai_cf (1. -. x) b a /. b

(* [cpl_student_t t nu] compute the "complement" of the Student's
   distribution: 1 - A(t|nu).  It is used to compute the significance
   of probabilistic tests. *)
let cpl_student_t t nu =
  betai (nu /. (nu +. t *. t)) (0.5 *. nu) 0.5


(* [comp_rates (name, bm)] computes the number, average and standard
   deviation of rates from the list of timings [bm].  If bm = [x(1);
   x(2);...; x(n)], the algorithm is

   m(1) = x(1)		m(k) = m(k-1) + (x(k) - m(k-1))/k
   s(1) = 0		s(k) = s(k-1) + (x(k) - m(k-1))(x(k) - m(k))

   One proves by recurrence that

   m(k) = sum(x(i) : 1 <= i <= k) / k
   s(k) = sum(x(i)**2 : 1 <= i <= k) - k m(k)**2
        = sum( (x(i) - m(k))**2 : 1 <= i <= k)

   Cf. Knuth, Seminumerical algorithms. *)
let comp_rates cpu (name, bm) =
  let rec loop n m s = function
    | [] -> (name, n, m, s)
    | b :: tl ->
        let rate = Int64.to_float b.iters /. cpu b in
        let n' = n + 1 in
        let m' = m +. (rate -. m) /. (float n') in
        let s' = s +. (rate -. m) *. (rate -. m') in
        loop n' m' s' tl in
  match bm with
  | [] -> (name, 0, nan, 0.) (* NaN used for no-data *)
  | b :: tl -> loop 1 (Int64.to_float b.iters /. (cpu b +. 1e-15)) 0. tl

(* Compare rates *)
let by_rates (_,_,r1,_) (_,_,r2,_) = compare (r1:float) r2

(* Check whether two rates are significantly different.  With a small
   [significance], a [true] returned value means that the rates are
   significantly different.  [n1] is the number of repetitions of the
   test1, [r1] is its mean rate and [s1] its standard deviation.
   [n2], [r2] and [s2] are similar for the test2. *)
let different_rates significance  n1 r1 s1  n2 r2 s2 =
  assert(n1 > 0 && n2 > 0);
  if n1 = 1 && n2 = 1 then true (* no info about distribution, assume
                                   they really are. *)
  else
    let df = float(n1 + n2 - 2) (* >= 1. *)
    and n1 = float n1
    and n2 = float n2 in
    let sD = sqrt((s1 +. s2) /. df *. (1. /. n1 +. 1. /. n2)) in
    let t = (r1 -. r2) /. sD in
    cpl_student_t t df <= significance


(* [string_of_rate display_as_rate confidence n r s] *)
let string_of_rate display_as_rate =
  let per_sec = if display_as_rate then "/s" else "" in
  fun confidence n r s ->
    (* Assume Gaussian distribution *)
    let sigma = sqrt(s/. float n) in
    let err = confidence *. sigma (* FIXME *) in
    let a, err =
      if display_as_rate then r, err else
        let n = 1. /. r in (n, n *. n *. err) (* Taylor of order 1 of 1/r *) in
    let p prec =
      if sigma < 1e-15 then (sprintf " %.*f%s" prec a per_sec, "")
      else (sprintf " %.*f+-" prec a, sprintf "%.*f%s" prec err per_sec) in
    if a >= 100. then p 0
    else if a >= 10. then p 1
    else if a >= 1.  then p 2
    else if a >= 0.1 then p 3
    else if sigma < 1e-15 then (sprintf " %g%s" a per_sec, "")
    else (sprintf " %g+-" a, sprintf "%g%s" err per_sec)


(* print results of a bench_many run *)
(* results = [(name, bm); (name, bm); (name, bm); ...] *)
let tabulate ?(no_parent=false) ?(confidence=0.95) results =
  if confidence < 0. || confidence > 1. then
    invalid_arg "Benchmark.tabulate: confidence < 0. or > 1.";
  let len = List.length results in
  if len = 0 then invalid_arg "Benchmark.tabulate: empty list of results";
  (* Compute (name, rate, sigma) for all results and sort them by rates *)
  let cpu = if no_parent then cpu_childs else cpu_process in
  let rates = List.sort by_rates (List.map (comp_rates cpu) results) in
  (* Decide whether to display by rates or seconds *)
  let display_as_rate =
    let (_,_,r,_) = List.nth rates (len / 2) in r > 1. in
  (*
   * Compute rows
   *)
  let top_row = "" :: (if display_as_rate then " Rate" else " s/iter")
                :: "" :: (List.map (fun (s,_,_,_) -> " " ^ s) rates) in
  (* Initialize the widths of the columns from the top row *)
  let col_width = Array.of_list (List.map String.length top_row) in
  (* Build all the data [rows], each starting with separation space *)
  let string_of_rate = string_of_rate display_as_rate in
  let make_row i (row_name, row_n, row_rate, row_s) =
    (* Column 0: test name *)
    col_width.(0) <- max (String.length row_name) col_width.(0);
    (* Column 1 & 2: performance *)
    let ra, ra_err = string_of_rate confidence row_n row_rate row_s in
    col_width.(1) <- max (String.length ra) col_width.(1);
    col_width.(2) <- max (String.length ra_err) col_width.(2);
    (* Columns 3..(len + 2): performance ratios *)
    let make_col j (_col_name, col_n, col_rate, col_s) =
      let ratio =
        if i = j || is_nan row_rate || is_nan col_rate then "--" else
          let p = 100. *. row_rate /. col_rate -. 100. in
          if p = 0. || different_rates (1. -. confidence)
            row_n row_rate row_s  col_n col_rate col_s
          then sprintf " %.0f%%" p
          else sprintf " [%.0f%%]" p in
      col_width.(j + 3) <- max (String.length ratio) col_width.(j + 3);
      ratio in
    row_name :: ra :: ra_err :: (list_mapi make_col rates) in
  let rows = list_mapi make_row rates in
  (*
   * Equalize column widths in the chart as much as possible without
   * exceeding 80 characters.  This does not use or affect cols 0, 1 and 2.
   *)
  (* Build an array of indexes [nth.(0..(len-1))] to access
     [col_width.(3..(len+2))] in nondecreasing order. *)
  let nth = Array.init len (fun i -> i + 3) in
  let by_width i1 i2 = compare col_width.(i1) col_width.(i2) in
  Array.sort by_width nth;
  let max_width = col_width.(nth.(len - 1)) in
  let rec stretcher min_width total =
    if min_width < max_width then stretch_min 0 min_width total
  and stretch_min i min_width total = (* try to stretch col [i] *)
    if total < 80 then begin
      if i < len && col_width.(nth.(i)) = min_width then begin
        col_width.(nth.(i)) <- col_width.(nth.(i)) + 1;
        stretch_min (i + 1) min_width (total + 1) (* stretch next col? *)
      end
      else stretcher (min_width + 1) total (* try again to stretch *)
    end in
  stretcher col_width.(nth.(0)) (Array.fold_left ( + ) 0 col_width);
  (*
   * Display the table
   *)
  let row_formatter row =
    list_iteri (fun i d -> printf "%*s" col_width.(i) d) row;
    print_string "\n" in
  row_formatter top_row;
  List.iter row_formatter rows;
  flush stdout

(** {2 Bench Tree} *)

module Tree = struct
  (** {2 Path} *)

  type path = string list

  let print_path_element fmt p =
    Format.pp_print_char fmt '.';
    Format.pp_print_cut fmt ();
    Format.pp_print_string fmt p

  let print_path fmt path =
    Format.fprintf fmt "@[<2>";
    (match path with
     | [] -> ()
     | [p] -> Format.pp_print_string fmt p
     | p :: tl -> Format.pp_print_string fmt p;
                 List.iter (print_path_element fmt) tl);
    Format.fprintf fmt "@]"

  (* Split the string along "." characters. Specification:
     assert (parse_path "foo.bar" = ["foo";"bar"]);
     assert (parse_path "foo" = ["foo"]);
     assert (parse_path "" = [""])
  *)
  let rev_parse_path check_name s =
    let l = ref [] in
    let i0 = ref 0 in
    for i = 0 to String.length s - 1 do
      if String.unsafe_get s i = '.' then (
        let name = String.sub s !i0 (i - !i0) in
        check_name name;
        l := name :: !l;
        i0 := i + 1;
      )
    done;
    let name = if !i0 = 0 then s
               else String.sub s !i0 (String.length s - !i0) in
    check_name name;
    name :: !l

  let check_reserved name =
    if name = "*" then invalid_arg "Name \"*\" is reserved for wildcard"

  let check_nothing _ = ()

  let parse_path s =
    List.rev(rev_parse_path check_nothing s)


  (** {2 Bench Tree} *)

  module SMap = Map.Make(String)

  (* A collection of benchmarks with fast concatenation. *)
  type benches = Single of samples Lazy.t
               | Pair of benches * benches

  let merge_benches_opt b1 b2 = match b1, b2 with
    | None, b | b, None -> b
    | Some b1, Some b2 -> Some(Pair(b1, b2))

  let rec number_of_benches = function
    | Single _ -> 1
    | Pair(b1, b2) -> number_of_benches b1 + number_of_benches b2

  let rec benches_iter benches ~f = match benches with
    | Single b -> f b
    | Pair(b1, b2) -> benches_iter b1 ~f;  benches_iter b2 ~f

  type t = Tree of benches option * t SMap.t
  (* benches at that level + named sublevels.  The name "" is
     understood as "at this level" and so will not be present in the
     map. *)

  let empty = Tree(None, SMap.empty)

  let is_empty (Tree(b, m)) =
    b = None && SMap.is_empty m

  let rec merge (Tree(b1, m1)) (Tree(b2, m2)) : t =
    let b = merge_benches_opt b1 b2 in
    let m = SMap.merge merge_opt m1 m2 in
    Tree(b, m)
  and merge_opt _ o1 o2 = match o1, o2 with
    | None, None -> None
    | Some o, None
    | None, Some o -> Some o
    | Some o1, Some o2 -> Some (merge o1 o2)

  let concat l = List.fold_left merge empty l

  let check_allowed_name n =
    if n = "*" then invalid_arg "Name \"*\" is reserved for wildcard";
    for i = 0 to String.length n - 1 do
      if String.unsafe_get n i = '.' then
        invalid_arg "Names cannot contain dots"
    done

  let of_bench bench = Tree(Some(Single bench), SMap.empty)

  let name_nonempty t n = Tree(None, SMap.singleton n t)

  let name t n =
    (* Assume the name [n] is valid *)
    if n = "" then t else name_nonempty t n

  (* prefix a tree with a path. Now the whole tree is only reachable
     from this given path *)
  let prefix path t =
    List.fold_right (fun n t -> check_reserved n; name t n) path t

  let ( @>> ) n t =
    let path = rev_parse_path check_reserved n in
    List.fold_left name t path

  let ( @> ) name bench = name @>> (of_bench bench)

  let (@>>>) n l = n @>> (concat l)

  let with_int f = function
    | [] -> empty
    | l ->
       let g i = Tree(None, SMap.singleton (string_of_int i) (f i)) in
       concat (List.map g l)

  (* print the structure of the tree, to show the user possible paths *)
  let rec print_tree_map fmt m =
    SMap.iter (print_tree_path fmt) m
  and print_tree_path fmt name (Tree(b, m)) =
    (match b with
     | None -> Format.fprintf fmt "@\n@[<2>- %s" name
     | Some b ->
        let n = number_of_benches b in
        Format.fprintf fmt "@\n@[<2>- %s: %i benchmark%s"
                       name n (if n > 1 then "s" else ""));
    print_tree_map fmt m;
    Format.fprintf fmt "@]"

  let print fmt (Tree(b, m)) =
    (match b with
     | None -> Format.fprintf fmt "No benchmark at root"
     | Some b ->
        let n = number_of_benches b in
        Format.fprintf fmt "%i benchmark%s at root"
                       n (if n > 1 then "s" else ""));
    print_tree_map fmt m

  (** {2 Selecting a subtree} *)

  let rec filter path (Tree(b,m) as t) = match path with
    | [] -> t
    | [""] -> (* Only return the benches at this level (no sub-levels) *)
       Tree(b, SMap.empty)
    | "" :: tl -> (* skip empty component NOT at the end, skip *)
       filter tl t
    | "*" :: tl ->
       (* wildcard pattern, select all subtrees *)
       let map_filter name t m =
         let t = filter tl t in
         (* Keep it only if not empty. *)
         if is_empty t then m else SMap.add name t m in
       Tree(b, SMap.fold map_filter m SMap.empty)
    | p0 :: tl ->
       match (try Some(SMap.find p0 m) with Not_found -> None) with
       | None -> empty
       | Some t -> let t = filter tl t in
                  (* propagate up the emptiness *)
                  if is_empty t then empty else name_nonempty t p0

  (** {2 Run} *)

  let print_sep fmt =
    Format.pp_print_string fmt "***********************************\
                                ***********************************";
    Format.pp_print_newline fmt ()

  let run_bench_path fmt is_previous_output rev_path = function
    | None -> is_previous_output
    | Some b ->
       if is_previous_output then print_sep fmt;
       Format.fprintf fmt "*** Run benchmarks for path \"%a\"@\n@."
                      print_path (List.rev rev_path);
       benches_iter b ~f:(fun b -> tabulate (Lazy.force b));
       true

  let rec run_all fmt is_previous_output rev_path (Tree(b, m)) =
    let is_previous_output =
      run_bench_path fmt is_previous_output rev_path b in
    SMap.fold (fun name t is_out -> run_all fmt is_out (name :: rev_path) t)
              m is_previous_output

  let run_1path fmt t is_previous_output path =
    (* Filtering the tree keep its full paths so we initialize
       [rev_path] to [[]]. *)
    run_all fmt is_previous_output [] (filter path t)

  let run_paths fmt ~paths t =
    let is_out = List.fold_left (run_1path fmt t) false paths in
    if not is_out then
      match paths with
      | [] -> Format.fprintf fmt "No benchmark to run.@\n@."
      | p0 :: tl ->
         Format.fprintf fmt "No benchmark to run for paths ";
         print_path fmt p0;
         List.iter (fun p -> print_path fmt p;
                          Format.pp_print_string fmt ", ") tl;
         Format.fprintf fmt ".@\n@."

  type arg_state = { mutable paths : path list;
                     mutable print_tree : bool;
                   }
  let arg () =
    let st = { paths = [];  print_tree = false } in
    let add_path s = st.paths <- parse_path s :: st.paths in
    let options = Arg.align
      [ "--path", Arg.String add_path, " only apply to subpath"
      ; "-p", Arg.String add_path, " short option for --path"
      ; "--all", Arg.Unit (fun () -> add_path "*"), " run all paths"
      ; "-a", Arg.Unit (fun () -> add_path "*"), " short option for --all"
      ; "--tree", Arg.Unit (fun () -> st.print_tree <- true), " print the tree"
      ] in
    st, options

  let run ?arg ?(paths=[]) ?(out=Format.std_formatter) t =
    match arg with
    | None -> run_paths out ~paths t
    | Some st ->
      if st.print_tree then
        Format.fprintf out "@[%a@]@." print t
      else
        run_paths out ~paths:(paths @ List.rev st.paths) t

  (** {2 Global Registration} *)

  (* the global tree of benchmarks *)
  let tree = ref empty

  let global () = !tree

  let register new_t =
    tree := merge !tree new_t

  let run_global ?(argv=Sys.argv) ?(out=Format.std_formatter) () =
    let st, specs = arg () in
    let no_anon _ = raise(Arg.Bad "No anonymous arguments allowed") in
    let pgm = try Filename.basename Sys.argv.(0)
              with _ -> "run benchmark" in
    let usage = pgm ^ " [options]" in
    try
      Arg.parse_argv argv specs no_anon usage;
      run ~arg:st ~out !tree
    with Arg.Bad msg | Arg.Help msg ->
      Format.fprintf out "%s@." msg
end

(* Local Variables: *)
(* compile-command: "make -k" *)
(* End: *)
