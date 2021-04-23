module StringMap = Map.Make(String)

type identifier = string

type op = string

type value = 
  | Ival of int
  | Aval of int * int
  | Pval of int 

type expr =
  | CstI of int
  | Var of identifier
  | BinOp of expr * expr * op
  | InlineIf of expr * expr * expr
  | Length of identifier
  | Base of identifier

type rhs =
  | Expr of expr
  | PtrRead of expr
  | ArrayRead of identifier * expr

type protect = Slh | Fence | Auto

type cmd =
  | Skip 
  | Fail
  | VarAssign of identifier * rhs
  | PtrAssign of expr * expr
  | ArrAssign of identifier * expr * expr
  | Seq of cmd * cmd
  | If of expr * cmd * cmd
  | While of expr * cmd
  | Protect of identifier * protect * rhs

(**         DIRECTIVES         **)
type prediction = bool

type directive =
  | Fetch
  | PFetch of prediction
  | Exec of int
  | Retire

(**         OBSERVATIONS         **)
type observation =
  | None
  | Read of int * int list
  | Write of int * int list
  | Fail of int
  | Rollback of int

(**        INSTRUCTION SET        **)
type instruction =
  | Nop
  | AssignV of identifier * value
  | AssignE of identifier * expr
  | Load of identifier * expr                   (*     id := load(e)         *)
  | Store of expr * expr
  | IProtect of identifier * protect * expr     (*     id := protect(e)     *)
  | Guard of expr * prediction * cmd list * int
  | Fail of int 

(**        CONFIGURATIONS         **)
type configuration = {
  is : instruction list ; 
  cs : cmd list ; 
  mutable mu : int array ; 
  mutable rho : int Map.Make(String).t ;
}

type decl_type =
  | TypI
  | TypA of int * int
  | TypP

(*	val eval: configuration ->
          decl_type Map.Make(String).t ->
          (configuration -> directive) ->
          observation list ->
          int -> (configuration * observation list * int)

	val blade: cmd -> cmd *)

let rec eval conf map attacker trace counter =

	match (conf.is, conf.cs) with
	| ([], []) -> (conf, trace, counter)
	| _ ->
	(
		let dir = attacker conf in

		let c = List.hd conf.cs in

		let istr = List.hd conf.is in

		let rec eval_fetch = 
			match c with
			| _ -> ()
		in

		let rec eval_pfetch p = 
			match c with
			| If(e, c1, c2) ->
				(if p then () else ())
			| _ -> failwith "Direttiva non valida!"
		in

		let rec eval_exec n = 
			let trvar_map lst m = () in ()
		in 

		let rec eval_retire = 
			match istr with
			| _ -> ()
		in

		match dir with
		| Fetch -> (conf, trace, counter)
		| PFetch(p) -> (conf, trace, counter)
		| Exec(n) -> (conf, trace, counter)
		| Retire -> (conf, trace, counter) 
	)

let print_cmd (c: cmd) : string =
	let rec helper_expr (e: expr) : string = match e with
	| CstI i -> Printf.sprintf "%d" i
	| Var n -> n
	| BinOp (e1, e2, op) -> Printf.sprintf "(%s %s %s)" (helper_expr e1) op (helper_expr e2)
	| InlineIf (e1, e2, e3) ->
		let s1 = helper_expr e1 in
		let s2 = helper_expr e2 in
		let s3 = helper_expr e3 in Printf.sprintf "(%s ? %s : %s)" s1 s2 s3
	| Length i -> Printf.sprintf "length(%s)" i
	| Base i -> Printf.sprintf "base(%s)" i
	in let helper_rhs (r: rhs) : string = match r with
	| Expr e -> helper_expr e
	| PtrRead e -> Printf.sprintf "*(%s)" (helper_expr e)
	| ArrayRead (i, e) -> Printf.sprintf "%s[%s]" i (helper_expr e)
	in let rec helper_cmd (c: cmd) : string = match c with
	| Skip -> "skip"
	| Fail -> "fail"
	| VarAssign (i, r) -> Printf.sprintf "%s = %s" i (helper_rhs r)
	| PtrAssign (e1, e2) -> Printf.sprintf "*(%s) = %s" (helper_expr e1) (helper_expr e2)
	| ArrAssign (i, e1, e2) -> Printf.sprintf "%s(%s) = %s" i (helper_expr e1) (helper_expr e2)
	| Seq (c1, c2) -> Printf.sprintf "%s;\n%s" (helper_cmd c1) (helper_cmd c2)
	| If (e, c1, c2) -> Printf.sprintf "if %s then\n%s\nelse\n%s\nendif" (helper_expr e) (helper_cmd c1) (helper_cmd c2)
	| While (e, c) -> Printf.sprintf "while %s do\n%s\nendwhile" (helper_expr e) (helper_cmd c)
	| Protect (i, p, r) ->
		let prot_type = match p with
		| Slh -> "protect_slh"
		| Fence -> "protect_fence"
		| Auto -> "protect"
		in Printf.sprintf "%s = %s(%s)" i prot_type (helper_rhs r)
	in helper_cmd c

type node_type =
	| ExprN of expr
	| RhsN of rhs
	| VarN of string

type 'n cmd_node =
	| Skip
	| Fail
	| VarAssign of identifier * rhs * 'n * 'n
	| PtrAssign of expr * expr
	| ArrAssign of identifier * expr * expr
	| Seq of 'n cmd_node * 'n cmd_node
	| If of expr * 'n cmd_node * 'n cmd_node
	| While of expr * 'n cmd_node
	| Protect of identifier * protect * rhs

let rec strip_nodes (c: 'n cmd_node) : (cmd) = match c with
	| Skip -> Skip
	| Fail -> Fail
	| VarAssign (i, r, _, _) -> VarAssign(i, r)
	| PtrAssign (e1, e2) -> PtrAssign (e1, e2)
	| ArrAssign (i, e1, e2) -> ArrAssign (i, e1, e2)
	| Seq (c1, c2) ->  Seq (strip_nodes c1, strip_nodes c2)
	| If (e, c1, c2) -> If (e, strip_nodes c1, strip_nodes c2)
	| While (e, c) -> While (e, strip_nodes c)
	| Protect (i, p, r) -> Protect (i, p, r)

open Graph
open Flow_network

let rec blade c =
	let module G = MatrixGraph in
	let module S = BFS(G) in
	let module N = FlowNetworkMaker (G) (S) in
	let var_nodes = Hashtbl.create 10 in
	let create_node (n: node_type) (g: node_type G.graph) : (node_type G.graph * G.node) =
		match Hashtbl.find_opt var_nodes n with
		| Some node -> (g, node)
		| None ->
			let (g, node) = G.add_node g n in
			Hashtbl.add var_nodes n node;
			(g, node)
	in let rec defuse_expr (e: expr) (g: node_type G.graph) (depth: int): (node_type G.graph * G.node) =
		let (g, new_node) = create_node (ExprN e) g in
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
		let (g, new_node) = create_node (RhsN r) g in
		match r with
		| Expr e -> defuse_expr e g depth
		| ArrayRead (i, e) ->
			let (g, expr_node) = defuse_expr e g depth in
			let g = G.connect g (expr_node, sink) max_int in
			let g = G.connect g (source, new_node) max_int in
			let g = G.connect g (expr_node, new_node) max_int in (g, new_node)
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
			(g, Seq(c1', c2'))
		| If (e, c1, c2) ->
			let g, enode = defuse_expr e g depth in
			let g, c1' = defuse_cmd c1 g depth in
			let g, c2' = defuse_cmd c2 g depth in
			let g = G.connect g (enode, sink) max_int in
			(g, If(e, c1', c2'))
		| While (e, body) ->
			let (g, enode) = defuse_expr e g depth in
			let g, body' = defuse_cmd body g (depth + 1) in
			let g = G.connect g (enode, sink) max_int in
			(g, While (e, body'))
		| VarAssign (name, r) ->
			let (g, varnode) = create_node (VarN name) g in
			let (g, rnode) = defuse_rhs r g depth in
			let g = G.connect g (varnode, rnode) 1 in
			(g, VarAssign (name, r, varnode, rnode))
		| PtrAssign (e1, e2) ->
			let (g, n1) = defuse_expr e1 g depth in
			let (g, n2) = defuse_expr e2 g depth in
			let g = G.connect g (n1, sink) max_int in
			(g, PtrAssign (e1, e2))
		| ArrAssign (i, e1, e2) ->
			let (g, n1) = defuse_expr e1 g depth in
			let (g, n2) = defuse_expr e2 g depth in
			let g = G.connect g (n1, sink) max_int in
			(g, ArrAssign(i, e1, e2))
		| Protect (name, prot, r) -> 
			let (g, n) = defuse_rhs r g depth in
			(g, Protect(name, prot, r))
		| Skip -> (g, Skip)
		| Fail -> (g, Fail)
	in let defuse_graph, annotated_cmd = defuse_cmd c (G.empty ()) 0
	in let min_cut = N.min_cut defuse_graph
	in let rec repair_cmd (c: G.node cmd_node) : G.node cmd_node =
		match c with
		| Skip -> Skip
		| Fail -> Fail
		| VarAssign (i, r, ni, nr) ->
			let ni_in_cut = List.exists (fun x -> x = ni) min_cut in
			let nr_in_cut = List.exists (fun x -> x = nr) min_cut in
			(match (ni_in_cut, nr_in_cut) with
			| false, true -> Protect (i, Auto, r)
			| true, false -> failwith "Invalid graph!"
			| _ -> VarAssign(i, r, ni, nr))
		| PtrAssign (e1, e2) -> PtrAssign (e1, e2)
		| ArrAssign (i, e1, e2) -> ArrAssign (i, e1, e2)
		| Seq (c1, c2) ->  Seq (repair_cmd c1, repair_cmd c2)
		| If (e, c1, c2) -> If (e, repair_cmd c1, repair_cmd c2)
		| While (e, c) -> While (e, repair_cmd c)
		| Protect (i, p, r) -> Protect (i, p, r)

	in repair_cmd annotated_cmd |> strip_nodes

let _ =
	let c : cmd = (Seq(
		VarAssign("x", ArrayRead("a", Var("i"))),
		If(Var("x"), Skip, Skip))) in
	Printf.printf "%s\n\n" (print_cmd c);
	let c' = blade c in
	Printf.printf "%s\n" (print_cmd c')