(* This is a typical problem where the functions are so fast (on a
   2Ghz machine) that it takes way too long to get results.  Thus a
   wrapping in a loop is done. *)

let n = 100

let string_of_month1 =
  let month = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug";
		 "Sep"; "Oct"; "Nov"; "Dec" |] in
  fun i -> Array.unsafe_get month i

let f1 () =
  for _ = 1 to n do ignore(string_of_month1 7) done

let string_of_month2 = function
  | 0 -> "Jan"
  | 1 -> "Feb"
  | 2 -> "Mar"
  | 3 -> "Apr"
  | 4 -> "May"
  | 5 -> "Jun"
  | 6 -> "Jul"
  | 7 -> "Aug"
  | 8 -> "Sep"
  | 9 -> "Oct"
  | 10 -> "Nov"
  | 11 -> "Dec"
  | _ -> failwith "h"

let f2 () =
  for _ = 1 to n do ignore(string_of_month2 7) done


open Benchmark

let () =
  let res = throughputN 3 ~repeat:5 [ ("arr", f1, ());
				      ("pat", f2, ()); ] in
  tabulate res;
  print_gc res

