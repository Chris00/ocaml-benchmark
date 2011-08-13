(* Compare bigarray and standard float array access times.  *)

open Bigarray

let n = 10_000
let m = 1000

(* Bigarrays
 ***********************************************************************)
type vec = (float, float64_elt, c_layout) Array1.t

let a = Array1.create float64 c_layout n
let () = Array1.fill a 1.

let ba (a: vec) =
  let s = ref 0. in
  for i = 0 to n-1 do s := !s +. a.{i} done

let ba_cl () =
  let s = ref 0. in
  for i = 0 to n-1 do s := !s +. a.{i} done

let ba_gen a =
  let s = ref 0. in
  for i = 0 to n-1 do s := !s +. a.{i} done

let ba_alloc () =
  let a = Array1.create float64 c_layout n in
  Array1.fill a 1.;
  let s = ref 0. in
  for j = 1 to m do
    for i = 0 to n-1 do s := !s +. a.{i} done
  done

let set_ba (a: vec) = for i = 0 to n-1 do a.{i} <- 1. done

let set_ba_alloc () =
  let a = Array1.create float64 c_layout n in
  for j = 1 to m do
    for i = 0 to n-1 do a.{i} <- 3. done
  done

let set_arr_alloc () =
  let a = Array.create n 0. in
  for j = 1 to m do
    for i = 0 to n-1 do a.(i) <- 3. done
  done

(* Arrays
 ***********************************************************************)
let b = Array.make n 1.

let arr (b: float array) =
  let s = ref 0. in
  for i = 0 to n-1 do s := !s +. b.(i) done

let arr_u (b: float array) =
	let s = ref 0. in
	for i = 0 to n -1 do s := !s +. (Array.unsafe_get b (i)) done

let arr_cl () =
  let s = ref 0. in
  for i = 0 to n-1 do s := !s +. b.(i) done

let arr_alloc () =
  let b = Array.make n 1. in
  let s = ref 0. in
  for j = 1 to m do
    for i = 0 to n-1 do s := !s +. b.(i) done
  done

let set_arr (b: float array) = for i = 0 to n-1 do b.(i) <- 1. done

(* Lists
 ***********************************************************************)
let c = Array.to_list b

let list c = ignore(List.fold_left ( +. ) 0. c)


open Benchmark

let () =
  let res = throughputN ~repeat:3 3
    [("ba", (fun () -> ba a), ());
     ("ba_cl", ba_cl, ());
     ("ba_gen", (fun () -> ba_gen a), ());
     ("set_ba", (fun () -> set_ba a), ());
     ("arr", (fun () -> arr b), ());
     ("arr_u", (fun () -> arr_u b), ());
     ("arr_cl", arr_cl, ());
     ("list", (fun () -> list c), ());
     ("set_arr", (fun () -> set_arr b), ());
    ] in
  tabulate res;

  let res = throughputN ~repeat:3 3
    [("ba", ba_alloc, ());
     ("set_ba", set_ba_alloc, ());
     ("arr", arr_alloc, ());
     ("set_arr", set_arr_alloc, ());
    ] in
  tabulate res;
