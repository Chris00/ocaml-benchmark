(* Tries to show the profile cost of composing small functions. *)

(* Small functions: permutations of [0 .. n-1] *)

let n = 100000
let rotate r = fun k -> (k + r) mod n
let reverse i j = fun k -> if i <= k && k <= j then j + i - k else k
let splice l i j = fun k ->
  if k < j then if k < i then k else k + l + 1
  else if k <= j + l then k - j + i
  else let k' = k - l - 1 in if k' < i then k' else k


open Benchmark

let ncomp = 400 (* Number of compositions *)

let make_perms =
  (* Create a random list of transformations *)
  Random.self_init();
  let rec random_perm ((p_f, p_v) as acc) i =
    if i <= 0 then acc else
      let c = Random.int 3 in
      (* New function *)
      let p =
        if c = 0 then rotate (Random.int n)
        else if c = 1 then reverse (Random.int n) (Random.int n)
        else (* c = 2 *) splice (Random.int n) (Random.int n) (Random.int n) in
      (* Corresponding array transformer *)
      let p_vec w v =
        for i = 0 to Array.length v - 1 do w.(i) <- p v.(i) done in
      random_perm (p :: p_f, p_vec :: p_v) (i - 1) in
  random_perm ([], [])

let () =
  let ncomp = 300 in
  let p_f, p_v = make_perms ncomp in
  let v = Array.init n (fun k -> k) in
  let do_f () =
    let f = List.fold_left (fun f f0 -> (fun k -> f0(f k))) (fun k -> k) p_f in
    Array.map f v
  and do_v () =
    snd(List.fold_left (fun (w,v) f -> f w v; (v,w)) (Array.make n 0, v) p_v)
  in

  let res = throughputN ~repeat:3 5 [("fun", do_f, ());
                                     ("vec", do_v, ()) ] in
  tabulate res;
  print_gc res
