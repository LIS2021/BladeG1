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
let lin_cost (d: int) (r: rhs): int = if d = 0 then 1 else 2 * d
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

module LinearCost : CostEstimator = struct
  let get_cost = lin_cost
end

module CostModelNoFences (E: CostEstimator) : CostEstimator = struct
  let get_cost = costly_fence E.get_cost
end

let cost_model = ref "uniform"
let costly_fences = ref false

let options = [("-c", Arg.Set_string cost_model, "Edge cost model. Can be 'uniform', 'exponential', 'linear'");
               ("-f", Arg.Set costly_fences, "Makes fences twice as expensive as SLHs")]

module G = MatrixGraph
module S = BFS (G)
module N = FlowNetworkMaker (G) (S)

let _ =
  Arg.parse options (fun _ -> ()) "Repairs a While program with protects";
  let (module E) = match !cost_model with
    | "uniform" -> (module UniformCost: CostEstimator)
    | "exponential" -> (module ExponentialCost : CostEstimator)
    | "linear" -> (module LinearCost : CostEstimator)
    | _ -> failwith "Invalid cost model"
  in let (module E) = if !costly_fences then
         (module CostModelNoFences (E) : CostEstimator)
       else
         (module E : CostEstimator)
  in let module Gen = HashtblGenerator (G) (E) in
  let module B = BaselineBlade (G) (S) (N) (Gen) in
  let c = parse_channel_fail stdin in
  let c' = B.repair_cmd c in
  Printf.printf "%s\n" (print_cmd c')
