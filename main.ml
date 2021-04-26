open Ast

open Opal
open Parser
open Blade
open Util
open Eval
open Graph
open Flow_network
open Def_use_generator

let uniform_cost (d: int) (r: rhs): int = 1
let exp_cost (d: int) (r: rhs): int = Int.shift_left 1 d
let costly_fence (f: int -> rhs -> int) (d: int) (r: rhs): int =
  let base_cost = f d r in
  match r with
  | ArrayRead(_, _) -> base_cost
  | _ -> base_cost * 2

module UniformCost : CostEstimator = struct
  let get_cost = uniform_cost
end

module ExponentialCost : CostEstimator = struct
  let get_cost = exp_cost
end

module ExponentialCostNoFences (E: CostEstimator) : CostEstimator = struct
  let get_cost = costly_fence E.get_cost
end

module G = MatrixGraph
module S = BFS (G)
module N = FlowNetworkMaker (G) (S)
module Gen = HashtblGenerator (G) (ExponentialCost)
module B = BaselineBlade (G) (S) (N) (Gen)

let _ =
  let c = parse_channel_fail stdin in
  let c' = B.repair_cmd c in
  Printf.printf "%s\n" (print_cmd c')
