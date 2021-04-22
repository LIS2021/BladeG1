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

let rec blade c =
	failwith "Not yet implemented"
