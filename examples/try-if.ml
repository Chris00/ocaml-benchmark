open Bigarray

let n = 100

let a = Array1.create float64 fortran_layout n

(* Base case: no test *)
let f0 () =
  for i = 1 to n do
    let x = a.{i} in
    ignore(x)
  done;
  ignore(0.)

let f1 () =
  for i = 1 to n+1 do
    let x = try a.{i} with _ -> 0. in
    ignore(x)
  done

let f2 () =
  for i = 1 to n+1 do
    let x = if i <= n then a.{i} else 0. in
    ignore(x)
  done

open Benchmark

let () =
  let res = throughputN ~repeat:5 3 [("None", f0, ());
                                     ("try", f1, ());
                                     ("if", f2, ())  ] in
  tabulate res
