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
  mutable rho : int StringMap.t ;
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

let fresh = ref 0 					(* Assign an id value to a node *)
let fresh() = 
  	incr fresh;
  	!fresh;;

let fresh_var = ref 0;;
let fresh_var() =
  incr fresh_var;
  !fresh_var;;

let make_fresh_var() =
  let new_var = "var" in
  let num = fresh_var() in
  new_var^(string_of_int num)


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
        			| Skip -> 
          				(
            					let ls = List.append conf.is [Nop] in
            					let cs_t = List.tl conf.cs in
            					{is=ls; cs=cs_t; mu=conf.mu; rho=conf.rho}
          				) 
        			| Fail -> 
          				(
            					let ls = List.append conf.is [Fail(fresh())] in
            					let cs_t = List.tl conf.cs in
            					{is=ls; cs=cs_t; mu=conf.mu; rho=conf.rho}
          				)
        			| VarAssign(id, rh) -> 
          				(
            					match rh with
            					| Expr(e) ->
              					(
                						let ls = List.append conf.is [AssignE(id, e)] in
                						let cs_t = List.tl conf.cs in
                						{is=ls; cs=cs_t; mu=conf.mu; rho=conf.rho}
              					)
            					| PtrRead(e) ->
              					(
                						let ls = List.append conf.is [Load(id, e)] in
                						let cs_t = List.tl conf.cs in
                						{is=ls; cs=cs_t; mu=conf.mu; rho=conf.rho}
              					)
            					| ArrayRead(id_arr, e) ->
              					(	
                						let g = BinOp(e, Length(id_arr), "<") in
                						let rhs_new = Expr(BinOp(Base(id_arr), e, "+")) in
                						let asgn_new = VarAssign(id, rhs_new) in
                						let cmd_new = If(g, asgn_new, Fail) in
                						let cs_t = List.tl conf.cs in
                						{is=conf.is; cs=cmd_new::cs_t; mu=conf.mu; rho=conf.rho}
              					)
          				)
        			| PtrAssign(e1, e2) -> 
          				(
            					let ls = List.append conf.is [Store(e1, e2)] in
            					let cs_t = List.tl conf.cs in
            					{is=ls; cs=cs_t; mu=conf.mu; rho=conf.rho}		
          				)
        			| ArrAssign(id, e1, e2) -> 
          				(
            					let g = BinOp(e1, Length(id), "<") in
            					let lhs_new = BinOp(Base(id), e1, "+") in
            					let asgn_new = PtrAssign(lhs_new, e2) in
            					let cmd_new = If(g, asgn_new, Fail) in
            					let cs_t = List.tl conf.cs in
            					{is=conf.is; cs=cmd_new::cs_t; mu=conf.mu; rho=conf.rho}
          				)
        			| Seq(c1, c2) -> 
          				(
            					let cs_t = List.tl conf.cs in
            					{is=conf.is; cs=c1::c2::cs_t; mu=conf.mu; rho=conf.rho}
          				)
        			| While(g, cm) -> 
          				(
            					let cs_t = List.tl conf.cs in
            					let cm_seq = Seq(cm, c) in
            					let w_unrolled = If(g, cm_seq, Skip) in
            					{is=conf.is; cs=w_unrolled::cs_t; mu=conf.mu; rho=conf.rho}
          				)
        			| Protect(id, pct, rhs) ->
          				(
            					match rhs with
            					| Expr(e) -> 
              						(
                							let ls = List.append conf.is [IProtect(id, pct, e)] in 
                							let cs_t = List.tl conf.cs in
                							{is=ls; cs=cs_t; mu=conf.mu; rho=conf.rho}
              						)
            					| ArrayRead(id_arr, e) when pct=Slh ->
              						(
                							let g = BinOp(e, Length(id), "<") in
                							let g_rhs = Expr(g) in
                							let v_frh = make_fresh_var() in
                							let c1 = VarAssign(v_frh, g_rhs) in
                							let ptr = BinOp(Base(id), e, "+") in
                							let xor = BinOp(ptr, g, "xor") in
                							let c3 = VarAssign(id, PtrRead(xor)) in
                							let c2 = VarAssign(v_frh, Expr(InlineIf(g, CstI(1), CstI(0)))) in
                							let cmd_new = Seq(c1, If(g, Seq(c2, c3), Fail)) in
                							let cs_t = List.tl conf.cs in
                							{is=conf.is; cs=cmd_new::cs_t; mu=conf.mu; rho=conf.rho}
              						) 
            					| _ -> 
              						(
                							let id_new = id^"'" in
                							let asgn_new = VarAssign(id_new, rhs) in 
                							let protect_new = Protect(id, pct, Expr(Var(id_new))) in
                							let cs_t = List.tl conf.cs in
                							{is=conf.is; cs=asgn_new::protect_new::cs_t; mu=conf.mu; rho=conf.rho}
              						)
          				)
        			| _ -> failwith "Direttiva non valida!"

      		in

      		let rec eval_pfetch p = 
        			match c with
        			| If(g, c1, c2) ->
          				(
            					let cs_t = List.tl conf.cs in
            					if p then let ls = List.append conf.is [Guard(g, p, c2::cs_t, fresh())] in
              						{is=ls; cs=c1::cs_t; mu=conf.mu; rho=conf.rho}
            					else let ls = List.append conf.is [Guard(g, p, c1::cs_t, 0)] in
              						{is=ls; cs=c2::cs_t; mu=conf.mu; rho=conf.rho}
          				)
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
