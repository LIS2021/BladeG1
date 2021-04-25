open Ast

type node_type =
  | ExprN of expr
  | RhsN of rhs
  | VarN of string

type 'n cmd_node =
  | SkipN
  | FailN
  | VarAssignN of identifier * rhs * 'n * 'n
  | PtrAssignN of expr * expr
  | ArrAssignN of identifier * expr * expr
  | SeqN of 'n cmd_node * 'n cmd_node
  | IfN of expr * 'n cmd_node * 'n cmd_node
  | WhileN of expr * 'n cmd_node
  | ProtectN of identifier * protect * rhs

let rec strip_nodes (c: 'n cmd_node) : (cmd) = match c with
  | SkipN -> Skip
  | FailN -> Fail
  | VarAssignN (i, r, _, _) -> VarAssign(i, r)
  | PtrAssignN (e1, e2) -> PtrAssign (e1, e2)
  | ArrAssignN (i, e1, e2) -> ArrAssign (i, e1, e2)
  | SeqN (c1, c2) ->  Seq (strip_nodes c1, strip_nodes c2)
  | IfN (e, c1, c2) -> If (e, strip_nodes c1, strip_nodes c2)
  | WhileN (e, c) -> While (e, strip_nodes c)
  | ProtectN (i, p, r) -> Protect (i, p, r)

open Graph
open Flow_network

let rec blade c =
  let module G = MatrixGraph in
  let module S = BFS(G) in
  let module N = FlowNetworkMaker (G) (S) in
  let var_nodes = Hashtbl.create 10 in
  let can_reach_sink g n = match (S.find_path g n) with
    | Left _ -> true
    | Right _ -> false
  in let create_node (n: node_type) (g: node_type G.graph) : (node_type G.graph * G.node) =
       match Hashtbl.find_opt var_nodes n with
       | Some node -> (g, node)
       | None ->
         let (g, node) = G.add_node g n in
         Hashtbl.add var_nodes n node;
         (g, node)
  in let rec defuse_expr (e: expr) (g: node_type G.graph) (depth: int): (node_type G.graph * G.node) =
       let ntype = match e with
         |Var name -> VarN name
         | _ -> ExprN e
       in let (g, new_node) = create_node ntype g in
       match e with
       | BinOp (e1, e2, op) ->
         let (g, n1) = defuse_expr e1 g depth in
         let (g, n2) = defuse_expr e2 g depth in
         let g = G.connect g (n1, new_node) max_int in
         let g = G.connect g (n2, new_node) max_int in (g, new_node)
       | InlineIf (e1, e2, e3) ->
         let (g, n1) = defuse_expr e1 g depth in
         let (g, n2) = defuse_expr e2 g depth in
         let (g, n3) = defuse_expr e3 g depth in
         let g = G.connect g (n1, new_node) max_int in
         let g = G.connect g (n2, new_node) max_int in
         let g = G.connect g (n3, new_node) max_int in (g, new_node)
       | _ -> (g, new_node)
  in let defuse_rhs (r: rhs) (g: node_type G.graph) (depth: int): (node_type G.graph * G.node) =
       let source = G.source g in
       let sink = G.sink g in
       let ntype = match r with
         | Expr e -> ExprN e
         | _ -> RhsN r
       in let (g, new_node) = create_node ntype g in
       match r with
       | Expr e -> defuse_expr e g depth
       | ArrayRead (i, e) ->
         let (g, expr_node) = defuse_expr e g depth in
         let g = G.connect g (expr_node, sink) max_int in
         let g = G.connect g (source, new_node) max_int in
         (g, new_node)
       | PtrRead e ->
         let (g, expr_node) = defuse_expr e g depth in
         let g = G.connect g (expr_node, sink) max_int in
         let g = G.connect g (source, new_node) max_int in
         let g = G.connect g (expr_node, new_node) max_int in (g, new_node)
  in let rec defuse_cmd (c: cmd) (g: node_type G.graph) (depth: int): (node_type G.graph * G.node cmd_node) =
       let sink = G.sink g in
       match c with
       | Seq (c1, c2) ->
         let g, c1' = defuse_cmd c1 g depth in
         let g, c2' = defuse_cmd c2 g depth in
         (g, SeqN(c1', c2'))
       | If (e, c1, c2) ->
         let g, enode = defuse_expr e g depth in
         let g, c1' = defuse_cmd c1 g depth in
         let g, c2' = defuse_cmd c2 g depth in
         let g = G.connect g (enode, sink) max_int in
         (g, IfN(e, c1', c2'))
       | While (e, body) ->
         let (g, enode) = defuse_expr e g depth in
         let g, body' = defuse_cmd body g (depth + 1) in
         let g = G.connect g (enode, sink) max_int in
         (g, WhileN (e, body'))
       | VarAssign (name, r) ->
         let (g, varnode) = create_node (VarN name) g in
         let (g, rnode) = defuse_rhs r g depth in
         let g = G.connect g (rnode, varnode) 1 in
         (g, VarAssignN (name, r, varnode, rnode))
       | PtrAssign (e1, e2) ->
         let (g, n1) = defuse_expr e1 g depth in
         let (g, n2) = defuse_expr e2 g depth in
         let g = G.connect g (n1, sink) max_int in
         (g, PtrAssignN (e1, e2))
       | ArrAssign (i, e1, e2) ->
         let (g, n1) = defuse_expr e1 g depth in
         let (g, n2) = defuse_expr e2 g depth in
         let g = G.connect g (n1, sink) max_int in
         (g, ArrAssignN(i, e1, e2))
       | Protect (name, prot, r) -> 
         let (g, n) = defuse_rhs r g depth in
         (g, ProtectN(name, prot, r))
       | Skip -> (g, SkipN)
       | Fail -> (g, FailN)
  in let defuse_graph, annotated_cmd = defuse_cmd c (G.empty ()) 0
  in let min_cut = N.min_cut defuse_graph
  in let residual_graph = G.disconnect_nodes defuse_graph min_cut
  in let rec repair_cmd (c: G.node cmd_node) : G.node cmd_node =
       match c with
       | SkipN -> SkipN
       | FailN -> FailN
       | VarAssignN (i, r, ni, nr) ->
         let ni_in_cut = List.mem ni min_cut in
         let nr_in_cut = List.mem nr min_cut in
         let reachable = can_reach_sink residual_graph ni in
         (match (ni_in_cut, nr_in_cut) with
          | false, true when reachable -> ProtectN (i, Auto, r)
          | true, false -> failwith "Invalid graph!"
          | _ -> VarAssignN(i, r, ni, nr))
       | PtrAssignN (e1, e2) -> PtrAssignN (e1, e2)
       | ArrAssignN (i, e1, e2) -> ArrAssignN (i, e1, e2)
       | SeqN (c1, c2) ->  SeqN (repair_cmd c1, repair_cmd c2)
       | IfN (e, c1, c2) -> IfN (e, repair_cmd c1, repair_cmd c2)
       | WhileN (e, c) -> WhileN (e, repair_cmd c)
       | ProtectN (i, p, r) -> ProtectN (i, p, r)

  in repair_cmd annotated_cmd |> strip_nodes