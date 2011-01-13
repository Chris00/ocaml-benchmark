(* Compare bigarray and standard float array access times.  *)

open Bigarray

let n = 10000

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

let b = Array.make n 1.

let arr (b: float array) =
  let s = ref 0. in
  for i = 0 to n-1 do s := !s +. b.(i) done

let arr_cl () =
  let s = ref 0. in
  for i = 0 to n-1 do s := !s +. b.(i) done


open Benchmark

let () =
  let res = throughputN ~repeat:5 3
    [("ba", (fun () -> ba a), ());
     ("ba_cl", ba_cl, ());
     ("ba_gen", (fun () -> ba_gen a), ());
     ("arr", (fun () -> arr b), ());
     ("arr_cl", arr_cl, ()) ] in
  tabulate res
