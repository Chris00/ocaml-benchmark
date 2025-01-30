(* Test that the test is performed correctly even when the test
   execution time exceeds the max throughput time. *)

open Benchmark

let long () =
  let s = ref 0. in
  for _ = 1 to 100_000 do
    for _ = 1 to 2_000 do
      s := !s +. 1.
    done
  done

let () =
  let t = throughputN 1 [ "long", long, () ] in
  tabulate t
