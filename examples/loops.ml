open Benchmark

(* Test for the speed of recusion w.r.t. imperative loops to access
   arrays fo floats. *)

let rec_loop (a : float array) =
  let rec loop i =
    if i < Array.length a then begin
      a.(i) <- a.(i) +. 1.;
      loop (i + 1)
    end in
  loop 0

let rec_loop2 (a : float array) =
  let len = Array.length a in
  let rec loop i =
    if i < len then begin
      a.(i) <- a.(i) +. 1.;
      loop (i + 1)
    end in
  loop 0

let for_loop (a : float array) =
  for i = 0 to Array.length a - 1 do
    a.(i) <- a.(i) +. 1.
  done

let () =
  let a = Array.make 100 1. in
  let res = throughputN ~repeat:5 1
              [("rec", rec_loop, a);
               ("rec2", rec_loop2, a);
               ("for", for_loop, a); ] in
  tabulate res;
  print_gc res
