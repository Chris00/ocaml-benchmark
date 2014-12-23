let () =
  let open Benchmark.Tree in
  "" @> lazy (let create() = Array.init 1_000_000 (fun i -> i) in
              Benchmark.latency1 18L create () )
  |> register;

  "map" @> lazy (let a = Array.init 1_000_000 (fun i -> i) in
                 let f x = x + 1 in
                 Benchmark.latency1 18L (Array.map f) a )
  |> register;

  "sort"
  @> lazy (let a = Array.init 1_000_000 (fun i -> -i) in
           Benchmark.latency1 18L (Array.sort compare) a )
 |> register;

  "sort" @>> "add"
  @> lazy (Benchmark.latency1 18L (fun x -> x + 1) 1)
  |> register

let () =
  Benchmark.Tree.run_global ()
