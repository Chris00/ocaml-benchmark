open Bigarray

let n = 1000

let a = Array1.create float64 fortran_layout n
let () = Array1.fill a 1.

let f1 () =
  let s = ref 0. in
  for i = 1 to n do s := !s +. a.{i} done

let b = Array.make n 1.

let f2 () =
  let s = ref 0. in
  for i = 0 to n-1 do s := !s +. b.(i) done


open Benchmark

let () =
  let res = throughputN ~repeat:5 3 [("ba", f1, ());
                                     ("arr", f2, ()) ] in
  tabulate res
