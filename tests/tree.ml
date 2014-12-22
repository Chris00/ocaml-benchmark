module T = Benchmark.Tree

let t0 =
  T.("" >: lazy (let create() = Array.init 1_000_000 (fun i -> i) in
                 Benchmark.latency1 18L create () ))
let t1 =
  T.("map"
     >: lazy (let a = Array.init 1_000_000 (fun i -> i) in
              let f x = x + 1 in
              Benchmark.latency1 18L (Array.map f) a ))

let t2 =
  T.("sort"
     >: lazy (let a = Array.init 1_000_000 (fun i -> -i) in
              Benchmark.latency1 18L (Array.sort compare) a ))

let t3 =
  T.("sort" >: "list"
     >: lazy (let a = List.init 1_000_000 (fun i -> -i) in
              Benchmark.latency1 18L (List.sort compare) a ))

let t = T.concat [t0; t1; t2; t3]

let () =
  T.run_main t
