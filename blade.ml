open Ast
open Graph
open Flow_network
open Def_use_generator

(** An implementation of the BLADE algorithm *)
module type BladeImpl = sig
  (** Performs the repair of a command using the BLADE algorithm *)
  val repair_cmd: cmd -> cmd
end

(** Implements the baseline BLADE algorithm *)
module BaselineBlade
    (G : Graph)
    (S : PathSearch
     with type path = G.edge list
      and type node = G.node
      and type 'a graph = 'a G.graph)
    (N : FlowNetwork
     with type node = G.node
      and type 'a graph = 'a G.graph)
    (Gen: DefUseGenerator
     with type node = G.node
      and type graph = node_type G.graph) : BladeImpl = struct
  let repair_cmd c =
    let gen = Gen.create in
    let can_reach_sink g n =
      match (S.find_path g n) with
      | Left _ -> true
      | Right _ -> false
    in let annotated_cmd = Gen.defuse_cmd gen c 0
    in let defuse_graph = Gen.get_graph gen
    in let min_cut = N.min_cut defuse_graph
    in let residual_graph = G.disconnect_nodes defuse_graph min_cut
    in let rec helper (c: G.node cmd_node) : G.node cmd_node =
         match c with
         | SkipN -> SkipN
         | FailN -> FailN
         | VarAssignN (i, r, ni, nr) ->
           let ni_in_cut = List.mem ni min_cut in
           let nr_in_cut = List.mem nr min_cut in
           let reachable = can_reach_sink residual_graph ni in
           let prot = match r with
             | ArrayRead(_, _) -> Slh
             | _ -> Fence
           in (match (ni_in_cut, nr_in_cut) with
               | false, true when reachable -> ProtectN (i, prot, r)
               | true, false -> failwith "Invalid graph!"
               | _ -> VarAssignN(i, r, ni, nr))
         | PtrAssignN (e1, e2) -> PtrAssignN (e1, e2)
         | ArrAssignN (i, e1, e2) -> ArrAssignN (i, e1, e2)
         | SeqN (c1, c2) ->  SeqN (helper c1, helper c2)
         | IfN (e, c1, c2) -> IfN (e, helper c1, helper c2)
         | WhileN (e, c) -> WhileN (e, helper c)
         | ProtectN (i, p, r) -> ProtectN (i, p, r)

    in helper annotated_cmd |> strip_nodes
end