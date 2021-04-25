open Ast

open Opal
open Parser
open Blade
open Util
open Eval
open Graph
open Flow_network
open Def_use_generator

module G = MatrixGraph
module S = BFS (G)
module N = FlowNetworkMaker (G) (S)
module Gen = HashtblGenerator (G)
module B = BaselineBlade (G) (S) (N) (Gen)

let _ =
  let c = parse_channel_fail stdin in
  let c' = B.repair_cmd c in
  Printf.printf "%s\n" (print_cmd c')
