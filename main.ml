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
  let src = LazyStream.of_channel stdin in
  match Parser.parse src with
  | None -> failwith "Syntax error"
  | Some c ->
    let c' = B.repair_cmd c in
    Printf.printf "%s\n" (print_cmd c')
