module F(E : sig
  val f : float -> float
  val g : float -> float
end) =
struct
  let h x = 1. +. E.f x +. E.g x
end

module A = F(struct let f x = x +. 1.  let g x = 2. *. x end)


type env = { f : float -> float;  g : float -> float }

let h_rec e x = 1. +. e.f x +. e.g x


let h_fun f g x = 1. +. f x +. g x

let f x = x +. 1.
let g x = 2. *. x

let h x = 1. +. f x +. g x


open Benchmark

let () =
  let res = throughputN ~repeat:3 3
    [("functor", (fun () -> A.h 1.), ());
     ("record", (fun () -> h_rec { f = f; g = g } 1.), ());
     ("fun arg", (fun () -> h_fun f g 1.), ());
     ("no arg", (fun () -> h 1.), ());
    ] in
  print_endline "Functor vesus records vesus passing as arg:";
  tabulate res;
  print_gc res

(* Local Variables: *)
(* compile-command: "make -k -C .." *)
(* End: *)
