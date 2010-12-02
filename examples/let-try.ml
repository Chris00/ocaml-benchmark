(* $Id: let-try.ml,v 1.1 2006-07-08 09:11:35 chris_77 Exp $ *)

(* Compare two possible implementations of let try x = ... with ... *)

let k x = if x >= 0 then x else failwith "x < 0"

let f a =
  let sgn s x =
    let y = (try Some(k x) with _ -> None) in
    match y with
    | None -> s
    | Some y -> s + y in
  ignore(Array.fold_left sgn 0 a)

let g a =
  let sgn s x =
    (try
       let y = k x in
       (fun () -> s + y)
     with _ ->
       (fun () -> s)
    )() in
  ignore(Array.fold_left sgn 0 a)


open Benchmark

let () =
  let a = Array.init 1000 (fun i -> Random.int 2 - 1) in
  let res = throughputN ~repeat:5 1 [("Some", f, a);
				     ("()->", g, a); ] in
  tabulate res
