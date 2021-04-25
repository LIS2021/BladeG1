open Ast
open Graph

(** Type for the nodes in the def-use graph *)
type node_type =
  | ExprN of expr
  | RhsN of rhs
  | VarN of string

(** Command where each assignment is annotated with the nodes of the left and right-hand side *)
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

(** Strips an annotated command of its annotations *)
let rec strip_nodes (c: 'n cmd_node) : cmd =
  match c with
  | SkipN -> Skip
  | FailN -> Fail
  | VarAssignN (i, r, _, _) -> VarAssign(i, r)
  | PtrAssignN (e1, e2) -> PtrAssign (e1, e2)
  | ArrAssignN (i, e1, e2) -> ArrAssign (i, e1, e2)
  | SeqN (c1, c2) ->  Seq (strip_nodes c1, strip_nodes c2)
  | IfN (e, c1, c2) -> If (e, strip_nodes c1, strip_nodes c2)
  | WhileN (e, c) -> While (e, strip_nodes c)
  | ProtectN (i, p, r) -> Protect (i, p, r)

(** A DefUseGenerator generates the def-use graph of a syntactical element *)
module type DefUseGenerator = sig
  type t
  type node
  type graph

  (** Creates a new empty DefUseGenerator *)
  val create: t

  (** Returns the def-use graph *)
  val get_graph: t -> graph

  (** Creates a new node for the specified element, or returns an existing one if already present *)
  val create_node: t -> node_type -> node

  (** Populates the def-use graph of an expression *)
  val defuse_expr: t -> expr -> int -> node

  (** Populates the def-use graph of the right-hand side of an assignment *)
  val defuse_rhs: t -> rhs -> int -> node

  (** Populates the def-use graph of a command *)
  val defuse_cmd: t -> cmd -> int -> node cmd_node
end

(** Generates a def-use graph using an Hashtbl implementation *)
module HashtblGenerator (G : Graph)
  : DefUseGenerator
    with type node = G.node
     and type graph = node_type G.graph = struct
  type graph = node_type G.graph
  type node = G.node
  type t = {
    tbl: (node_type, node) Hashtbl.t;
    g: graph; }

  let create = { tbl = Hashtbl.create 10; g = G.empty () }

  let get_graph (gen: t) : graph = gen.g

  let create_node (gen: t) (n: node_type) : node =
    match Hashtbl.find_opt gen.tbl n with
    | Some node -> node
    | None ->
      let (g, node) = G.add_node gen.g n in
      Hashtbl.add gen.tbl n node;
      node

  let rec defuse_expr (gen: t) (e: expr) (depth: int): node =
    let ntype = match e with
      |Var name -> VarN name
      | _ -> ExprN e
    in let new_node = create_node gen ntype in
    match e with
    | BinOp (e1, e2, op) ->
      let n1 = defuse_expr gen e1 depth in
      let n2 = defuse_expr gen e2 depth in
      let _ = G.connect gen.g (n1, new_node) max_int in
      let _ = G.connect gen.g (n2, new_node) max_int in new_node
    | InlineIf (e1, e2, e3) ->
      let n1 = defuse_expr gen e1 depth in
      let n2 = defuse_expr gen e2 depth in
      let n3 = defuse_expr gen e3 depth in
      let _ = G.connect gen.g (n1, new_node) max_int in
      let _ = G.connect gen.g (n2, new_node) max_int in
      let _ = G.connect gen.g (n3, new_node) max_int in new_node
    | _ -> new_node

  let defuse_rhs (gen: t) (r: rhs) (depth: int): node =
    let source = G.source gen.g in
    let sink = G.sink gen.g in
    let ntype = match r with
      | Expr e -> ExprN e
      | _ -> RhsN r
    in let new_node = create_node gen ntype in
    match r with
    | Expr e -> defuse_expr gen e depth
    | ArrayRead (i, e) ->
      let expr_node = defuse_expr gen e depth in
      let _ = G.connect gen.g (expr_node, sink) max_int in
      let _ = G.connect gen.g (source, new_node) max_int in
      new_node
    | PtrRead e ->
      let expr_node = defuse_expr gen e depth in
      let _ = G.connect gen.g (expr_node, sink) max_int in
      let _ = G.connect gen.g (source, new_node) max_int in
      let _ = G.connect gen.g (expr_node, new_node) max_int in
      new_node

  let rec defuse_cmd (gen: t) (c: cmd) (depth: int): node cmd_node =
    let sink = G.sink gen.g in
    match c with
    | Seq (c1, c2) ->
      let c1' = defuse_cmd gen c1 depth in
      let c2' = defuse_cmd gen c2 depth in
      SeqN(c1', c2')
    | If (e, c1, c2) ->
      let enode = defuse_expr gen e depth in
      let c1' = defuse_cmd gen c1 depth in
      let c2' = defuse_cmd gen c2 depth in
      let _ = G.connect gen.g (enode, sink) max_int in
      IfN(e, c1', c2')
    | While (e, body) ->
      let enode = defuse_expr gen e depth in
      let body' = defuse_cmd gen body (depth + 1) in
      let _ = G.connect gen.g (enode, sink) max_int in
      WhileN (e, body')
    | VarAssign (name, r) ->
      let varnode = create_node gen (VarN name) in
      let rnode = defuse_rhs gen r depth in
      let _ = G.connect gen.g (rnode, varnode) 1 in
      VarAssignN (name, r, varnode, rnode)
    | PtrAssign (e1, e2) ->
      let n1 = defuse_expr gen e1 depth in
      let _ = defuse_expr gen e2 depth in
      let _ = G.connect gen.g (n1, sink) max_int in
      PtrAssignN (e1, e2)
    | ArrAssign (i, e1, e2) ->
      let n1 = defuse_expr gen e1 depth in
      let _ = defuse_expr gen e2 depth in
      let _ = G.connect gen.g (n1, sink) max_int in
      ArrAssignN(i, e1, e2)
    | Protect (name, prot, r) ->
      let _ = defuse_rhs gen r depth in
      ProtectN(name, prot, r)
    | Skip -> SkipN
    | Fail -> FailN
end