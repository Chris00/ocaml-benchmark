open Bigarray

let n = 1_000

(* Bigarrays *)
type vec = (float, float64_elt, c_layout) Array1.t

let a = Array1.create float64 c_layout n
let () = Array1.fill a 1.

let ba f (x : vec) =
  for i = 0 to n - 1 do
    f x.{i}
  done

let ba_unsafe f (x : vec) =
  for i = 0 to n - 1 do
    f (Array1.unsafe_get x i)
  done

(* Arrays *)
let b = Array.make n 1.

let arr f (x : float array) =
  for i = 0 to n - 1 do
    f x.(i)
  done

let arr_unsafe f (x : float array) =
  for i = 0 to n - 1 do
    f (Array.unsafe_get x i)
  done

(* Lists *)
let c = Array.to_list b

open Benchmark

let () =
  (* Simulate a simple side effect *)
  let z = ref 0. in
  let f x = z := x in

  let res =
    throughputN ~repeat:3 3
      [
        "ba", (fun () -> ba f a), ();
        "ba_unsafe", (fun () -> ba_unsafe f a), ();
        "arr", (fun () -> arr f b), ();
        "arr_unsafe", (fun () -> arr_unsafe f b), ();
        "list", (fun () -> List.iter f c), ();
      ]
  in
  print_endline "Iterating a function with a simple side effect:";
  tabulate res
