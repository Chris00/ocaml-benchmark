(* File: benchmark.ml
   For comparing runtime of functions
   *********************************************************************

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
(* $Id: benchmark.ml,v 1.6 2004-08-20 18:11:05 chris_77 Exp $ *)

open Printf

type t = {
  wall   : float;
  utime  : float;
  stime  : float;
  cutime : float;
  cstime : float;
  iters  : int;
}

type style = No_child | No_parent | All | Auto | Nil

let null_t =
  { wall = 0.; utime = 0.; stime = 0.; cutime = 0.; cstime = 0.; iters = 0 }

let make n =
  let tms = Unix.times () in
  { wall = Unix.time ();
    utime = tms.Unix.tms_utime;   stime = tms.Unix.tms_stime;
    cutime = tms.Unix.tms_cutime; cstime = tms.Unix.tms_cstime;
    iters = n }

let add a b =
  { wall = a.wall +. b.wall; utime = a.utime +. b.utime;
    stime = a.stime +. b.stime; cutime = a.cutime +. b.cutime;
    cstime = a.cstime +. b.cstime; iters = a.iters + b.iters }

let sub a b =
  { wall = a.wall -. b.wall; utime = a.utime -. b.utime;
    stime = a.stime -. b.stime; cutime = a.cutime -. b.cutime;
    cstime = a.cstime -. b.cstime; iters = a.iters - b.iters }

let cpu_p b = b.utime +. b.stime
let cpu_c b = b.cutime +. b.cstime
let cpu_a b = b.utime +. b.stime +. b.cutime +. b.cstime

(* Return a formatted string representation of benchmark structure
   according to [style].  Default presentation parameters set here. *)
let to_string ?(style = Auto) ?(fwidth = 5) ?(fdigits = 2) b =
  let pt = cpu_p b
  and ct = cpu_c b in
  let style = (if style = Auto then if ct > 1e-10 then All else No_child
	       else style) in
  let iter_info t =
    if b.iters > 0 && t > 0.0 then
      sprintf " @ %*.*f/s (n=%d)" fwidth fdigits (float b.iters /. t) b.iters
    else "" in
  let f x = sprintf "%*.*f" fwidth fdigits x in
  match style with
  | All ->
      sprintf "%2.0f WALL (%s usr %s sys + %s cusr %s csys = %s CPU)%s"
        b.wall (f b.utime) (f b.stime) (f b.cutime) (f b.cstime)
        (f(pt +. ct)) (iter_info pt)
  | No_child ->
      sprintf "%2.0f WALL (%s usr + %s sys = %s CPU)%s"
        b.wall (f b.utime) (f b.stime) (f pt) (iter_info pt)
  | No_parent ->
      sprintf "%2.0f WALL (%s cusr + %s csys = %s CPU)%s"
        b.wall (f b.cutime) (f b.cstime) (f ct) (iter_info ct)
  | Nil -> ""
  | Auto -> assert false


type samples = (string * t list) list

let by_name (s1, _) (s2, _) = compare (s1:string) s2

let merge (l1:samples) l2 =
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


(* run function f count times, return time taken *)
let timeit n f x =
  let runloop n_iters f x =
    let tbase = (make 0).utime in
    (* Wait for user timer to tick.  This makes the error range more
       like -0.01, +0.  If we don't wait, then it's more like -0.01,
       +0.01. *)
    let t0 = ref (make 0) in
    while tbase = (!t0).utime do t0 := make 0 done;
    (* loop over function we are timing [n] times *)
    for i = 1 to n do ignore(f x) done;
    let t1 = make n_iters in
    sub t1 !t0 in		  (* return the elapsed time *)
  let wn = runloop 0 ignore () in (* time a null-loop; no iter count *)
  let wc = runloop n f x in
  sub wc wn                       (* time of function minus null-loop *)

let latency ?(repeat=1) n f x =
  let rec loop nrep acc =
    if nrep < 1 then acc
    else loop (nrep - 1) (timeit n f x :: acc) in
  loop repeat []


(* Perl: countit *)
(* Read the code from bottom to top: [min_iter] determines the minimal
   number of iterations to have a significant timing, then
   [estimate_niter] estimate by linear interpolation the number of
   iter to run [> tmax] and then the test is performed. *)
let throughput ?(repeat=1) tmax f x =
  (* Run [f] for [niter] times and complete with >= [nmin] iterations
     (estimated by linear interpolation) to run >= [tmax]. *)
  let rec run_test nmin niter bm_init =
    let bm = add bm_init (timeit niter f x) in
    let tn = cpu_p bm in
    if tn >= tmax then bm else
      let n = truncate((tmax /. tn -. 1.) *. float bm.iters) in
      run_test nmin (max nmin n) bm  in
  (* Repeat the test [nrep] times. *)
  let rec repeat_test nrep acc nmin niter =
    if nrep < 1 then acc else
      let bm = run_test nmin niter null_t in
      repeat_test (nrep - 1) (bm :: acc) nmin niter in
  (* Estimate number of iter > [nmin] to have a running time >=
     [tmax].  The initial estimate is [n] running [tn] secs.  Linear
     estimates bear a 5% fudge to improve the overall responsiveness. *)
  let tpra = 0.1 *. tmax (* Target/time practice *) in
  let rec estimate_niter nmin n tn =
    if tn >= tpra then
      let niter = truncate(float n *. (1.05 *. tmax /. tn)) (* lin estim *) in
      repeat_test repeat [] nmin (max nmin niter)
    else
      let new_n = truncate(float n *. 1.05 *. tpra /. tn) (* lin estim *) in
      let new_tn = cpu_p (timeit new_n f x) in
      let n = (* make sure we make progress *)
        if new_tn > 1.2 *. tn then new_n
        else truncate(1.1 *. float n) + 1 in
      estimate_niter nmin n new_tn in
  (* Determine the minimum number of iterations to run > 0.1 sec *)
  let rec min_iter n =
    let bm = timeit n f x in
    let tn = cpu_p bm in
    if tn <= 0.1 then min_iter(2 * n)
    else if tn < tmax then estimate_niter n n tn (* tn > 0.1 *)
    else (* minimal [n] good for [tmax] *)
      repeat_test (repeat - 1) [bm] n n in
  min_iter 1



(* [print1 bm title] prints the list of timings [bm] identified with
   with [name] according to the style defined by the optional
   parameters. *)
let print1 ?(min_count=4) ?(min_cpu=0.4) ?(style=Auto) ?fwidth ?fdigits
  bm name =
  if style <> Nil then begin
    let print_t prefix b =
      printf "%10s: %s\n" prefix (to_string ~style ?fwidth ?fdigits b);
      if b.iters < min_count || (b.wall < 1. && b.iters < 1000)
        || cpu_a b < min_cpu
      then print_string
        "            (warning: too few iterations for a reliable count)\n" in
    begin match bm with
    | [] -> printf "%10s: (no results)\n" name
    | b :: tl ->
        print_t name b;
        List.iter (print_t "") tl
    end;
    flush stdout
  end

(* Perl: timethese *)
let testN ~title ~test default_f_name
  ?min_count ?min_cpu ?(style=Auto) ?fwidth ?fdigits funs =
  if style <> Nil then begin
    let names = List.map (fun (a,_,_) -> a) funs in
    print_endline(title(String.concat ", " names));
  end;
  let result_of (name, f, x) =
    let bm = test f x in
    if style <> Nil then begin
      let name = if name = "" then default_f_name else name in
      print1 ?min_count ?min_cpu ~style ?fwidth ?fdigits bm name
    end;
    (name, bm) in
  List.map result_of funs


let latencyN ?min_count ?min_cpu ?style ?fwidth ?fdigits ?(repeat=1) n funs =
  let title s =
    sprintf "Latencies for %d iterations of %s%s:" n s
      (if repeat > 1 then sprintf " (%i runs)" repeat else "") in
  testN ~title ~test:(latency ~repeat n)
    (sprintf "[run %d times]" n)
    ?min_count ?min_cpu ?style ?fwidth ?fdigits funs

let latency1 ?min_count ?min_cpu ?(name="") ?style ?fwidth ?fdigits
  ?repeat  n f x =
  latencyN ?min_count ?min_cpu ?style ?fwidth ?fdigits ?repeat n [(name, f, x)]

let throughputN ?min_count ?min_cpu ?style ?fwidth ?fdigits ?(repeat=1)
  n funs =
  let tmax = if n <= 0 then 3. (* default num of sec *) else float n in
  let title s =
    sprintf "Throughputs for %s%s running%s for at least %g CPU seconds:"
      s (if List.length funs > 1 then ", each" else "")
      (if repeat > 1 then sprintf " %i times" repeat else "")
      tmax in
  testN ~title ~test:(throughput ~repeat tmax)
    (sprintf "[run > %3.1g secs]" tmax)
    ?min_count ?min_cpu ?style ?fwidth ?fdigits funs

let throughput1 ?min_count ?min_cpu ?(name="") ?style ?fwidth ?fdigits
  ?repeat n f x =
  throughputN ?min_count ?min_cpu ?style ?fwidth ?fdigits ?repeat
    n [(name, f, x)]



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
let beta a b =
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
    (* Odd red step d_2m+1 *)
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
   distribution: 1 - A(t|nu).  It tells is used to compute the
   significance of probabilitic tests. *)
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
        let rate = (float b.iters) /. (cpu b +. 1e-15) in
        let n' = n + 1 in
        let m' = m +. (rate -. m) /. (float n') in
        let s' = s +. (rate -. m) *. (rate -. m') in
        loop n' m' s' tl in
  match bm with
  | [] -> (name, 0, nan, 0.) (* NaN used for no-data *)
  | b :: tl -> loop 1 ((float b.iters) /. (cpu b +. 1e-15)) 0. tl

(* Compare rates *)
let by_rates (_,_,r1,_) (_,_,r2,_) = compare (r1:float) r2

(* Check whether two rates are significantly different. *)
let different_rates significance  n1 r1 s1  n2 r2 s2 =
  assert(n1 > 0 && n2 > 0);
  if n1 = 1 && n2 = 1 then true (* no info about distribution *)
  else
    let df = float(n1 + n2 - 2) (* >= 1. *)
    and n1 = float n1
    and n2 = float n2 in
    let sD = sqrt((s1 +. s2) /. df *. (1. /. n1 +. 1. /. n2)) in
    let t = (r1 -. r2) /. sD in
    cpl_student_t t df <= significance


(* [string_of_rate display_as_rate r s] *)
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
      if sigma < 1e-15 then (sprintf " %0.*f%s" prec a per_sec, "")
      else (sprintf " %0.*f+-" prec a, sprintf "%.*f%s" prec err per_sec) in
    if a >= 100. then p 0
    else if a >= 10. then p 1
    else if a >= 1.  then p 2
    else if a >= 0.1 then p 3
    else if sigma < 1e-15 then (sprintf " %g%s" a per_sec, "")
    else (sprintf " %g+-" a, sprintf "%g%s" err per_sec)


(* print results of a bench_many run *)
(* results = [(name, bm); (name, bm); (name, bm); ...] *)
(* Perl: cmpthese *)
let tabulate ?(no_parent=false) ?(confidence=0.95) results =
  let len = List.length results in
  if len = 0 then invalid_arg "Benchmark.tabulate";
  (* Compute (name, rate, sigma) for all results and sort them by rates *)
  let cpu = if no_parent then cpu_c else cpu_p in
  let rates = List.sort by_rates (List.map (comp_rates cpu) results) in
  (* Decide whether to display by rates or seconds *)
  let display_as_rate =
    let (_,_,r,_) = List.nth rates (len / 2) in r > 1. in
  (*
   * Compute rows
   *)
  let top_row = "" :: (if display_as_rate then " Rate" else " s/iter") ::
                  "" :: (List.map (fun (s,_,_,_) -> " " ^ s) rates) in
  (* Initialize the widths of the columns from the top row *)
  let col_width = Array.of_list (List.map String.length top_row) in
  (* Build all the data [rows], each starting with separation space *)
  let make_row i (row_name, row_n, row_rate, row_s) =
    (* Column 0: test name *)
    col_width.(0) <- max (String.length row_name) col_width.(0);
    (* Column 1 & 2: performance *)
    let ra, ra_err =
      string_of_rate display_as_rate confidence row_n row_rate row_s in
    col_width.(1) <- max (String.length ra) col_width.(1);
    col_width.(2) <- max (String.length ra_err) col_width.(2);
    (* Columns 3..(len + 2): performance ratios *)
    let make_col j (col_name, col_n, col_rate, col_s) =
      let ratio =
        if i = j || is_nan row_rate || is_nan col_rate then "--" else
          let p = 100. *. row_rate /. col_rate -. 100. in
          if different_rates (1. -. confidence)
            row_n row_rate row_s  col_n col_rate col_s
          then sprintf " %.0f%%" p
          else sprintf " [%.0f%%]" p in
      col_width.(j + 3) <- max (String.length ratio) col_width.(j + 3);
      ratio in
    row_name :: ra :: ra_err :: (list_mapi make_col rates) in
  let rows = list_mapi make_row rates in
  (*
   * Equalize column widths in the chart as much as possible without
   * exceeding 80 characters.  This does not use or affect cols 0 or 1.
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
