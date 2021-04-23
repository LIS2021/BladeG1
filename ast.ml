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
open Graph
open Flow_network

let rec blade c =
	let module G = MatrixGraph in
	let module S = BFS(G) in
	let module N = FlowNetworkMaker (G) (S) in
	let var_nodes = Hashtbl.create 10 in
	let create_node (r: rhs) (g: rhs G.graph) : (rhs G.graph * G.node) =
		match Hashtbl.find_opt var_nodes r with
		| Some node -> (g, node)
		| None ->
			let (g, node) = G.add_node g r in
			Hashtbl.add var_nodes r node;
			(g, node)
	in let rec blade_expr (e: expr) (g: rhs G.graph) (depth: int): (rhs G.graph * G.node) =
		let (g, new_node) = create_node (Expr e) g in
		match e with
		| BinOp (e1, e2, op) ->
			let (g, n1) = blade_expr e1 g depth in
			let (g, n2) = blade_expr e2 g depth in
			let g = G.connect g (n1, new_node) 1 in
			let g = G.connect g (n2, new_node) 1 in (g, new_node)
		| InlineIf (e1, e2, e3) ->
			let (g, n1) = blade_expr e1 g depth in
			let (g, n2) = blade_expr e2 g depth in
			let (g, n3) = blade_expr e3 g depth in
			let g = G.connect g (n1, new_node) 1 in
			let g = G.connect g (n2, new_node) 1 in
			let g = G.connect g (n3, new_node) 1 in (g, new_node)
		| _ -> (g, new_node)
	in let blade_rhs (r: rhs) (g: rhs G.graph) (depth: int): (rhs G.graph * G.node) =
		let source = G.source g in
		let sink = G.sink g in
		let (g, new_node) = create_node r g in
		match r with
		| Expr e -> blade_expr e g depth
		| ArrayRead (i, e) ->
			let (g, expr_node) = blade_expr e g depth in
			let g = G.connect g (expr_node, sink) 1 in
			let g = G.connect g (source, new_node) 1 in
			let g = G.connect g (expr_node, new_node) 1 in (g, new_node)
		| PtrRead e ->
			let (g, expr_node) = blade_expr e g depth in
			let g = G.connect g (expr_node, sink) 1 in
			let g = G.connect g (source, new_node) 1 in
			let g = G.connect g (expr_node, new_node) 1 in (g, new_node)
	in let rec blade_cmd (c: cmd) (g: rhs G.graph) (depth: int): rhs G.graph =
		let sink = G.sink g in
		match c with
		| Skip | Fail -> g
		| Seq (c1, c2) ->
			let g = blade_cmd c1 g depth in
			blade_cmd c2 g depth
		| If (e, c1, c2) ->
			let (g, enode) = blade_expr e g depth in
			let g = blade_cmd c1 g depth in
			let g = blade_cmd c2 g depth in
			G.connect g (enode, sink) 1
		| While (e, body) ->
			let (g, enode) = blade_expr e g depth in
			let g = blade_cmd body g (depth + 1) in
			G.connect g (enode, sink) 1
		| VarAssign (name, r) ->
			let (g, varnode) = create_node (Expr (Var name)) g in
			let (g, rnode) = blade_rhs r g depth in
			G.connect g (varnode, rnode) 1
		| PtrAssign (e1, e2) ->
			let (g, n1) = blade_expr e1 g depth in
			let (g, n2) = blade_expr e2 g depth in
			G.connect g (n1, sink) 1
		| ArrAssign (i, e1, e2) ->
			let (g, n1) = blade_expr e1 g depth in
			let (g, n2) = blade_expr e2 g depth in
			G.connect g (n1, sink) 1
		| Protect (name, prot, r) -> 
			let (g, n) = blade_rhs r g depth in g

	in failwith ""
