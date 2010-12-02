let string_of_month =
  let month = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug";
		 "Sep"; "Oct"; "Nov"; "Dec" |] in
  fun i -> Array.unsafe_get month i

let string_of_month1 = function
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

open Benchmark

let () =
  let res = latencyN 20_000_000 [ ("arr", string_of_month, 7);
				  ("pat", string_of_month1, 7); ] in
  tabulate res

