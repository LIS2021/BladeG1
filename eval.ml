open Ast
open Vm_types
open Util

(*  val eval: configuration ->
          decl_type Map.Make(String).t ->
          (configuration -> directive) ->
          observation list ->
          int -> (configuration * observation list * int)

    val blade: cmd -> cmd *)

let fresh = ref 0           
let fresh_var = ref 0;;

(** 
   This function assign a fresh number p to a specific [instruction] type value (i.e. Fail(p) and Guard(_,_,_, p)) 
   @return a new int value
*)
let fresh() = 
  incr fresh;
  !fresh;;

(** 
   This function assign a fresh id number  
   @return a new int value
*)
let fresh_var() =
  incr fresh_var;
  !fresh_var;;

(** 
   This function creates a variable having the form "var"^id (e.g. "var0", "var1", ...) 
   @return a new variable identifier   
*)
let make_fresh_var() =
  let new_var = "var" in
  let num = fresh_var() in
  "\'"^new_var^(string_of_int num)

(** Evaluation of expressions
            @param e    the expression
            @param rho  the environment 
            @param map  map of declared variables
            @return value the value of the expression
*)
let rec eval_expr e rho map = 
  match e with
  | CstI(i)           -> Ival(i)
  | Var(id)           -> (let id1 = StringMap.find_opt id rho in
                          let id2 = StringMap.find_opt id map in
                          match (id1, id2) with
                          |(_,Some(TypA(b,l))) -> Aval(b,l)
                          |(Some(x),_)   -> Ival(x)
                          |(_,_) -> failwith "Identifier not declared" )             
  | BinOp(e1,e2,op)   -> (let e1' = eval_expr e1 rho map in
                          let e2' = eval_expr e2 rho map in          
                          match (op,e1',e2') with
                          |("+", Ival(x), Ival(y)) -> Ival(x + y)
                          |("*", Ival(x), Ival(y)) -> Ival(x * y)
                          |("<", Ival(x), Ival(y)) -> Ival(Bool.to_int(x < y))
                          |("&", Ival(x), Ival(y)) -> Ival(x land y)
                          |("^", Ival(x), Ival(y)) -> Ival(x lxor y)
                          |_ -> failwith "Invalid type for binary operation")
  | InlineIf(e,e1,e2) -> (let e' = eval_expr e rho map in
                          match e' with
                          |Ival(b) -> if b >= 1 then eval_expr e1 rho map else eval_expr e2 rho map
                          |_       -> failwith "Invalid type for guard")
  | Length(a)         -> (let t = StringMap.find_opt a map in
                          match t with
                          |Some(TypA(b,l)) -> Ival(l)
                          |Some(TypI) -> failwith (Printf.sprintf "Invalid type for lenght: %s is int" a)
                          |None -> failwith (Printf.sprintf "Invalid type for length: %s is unknown" a))
  | Base(a)           -> (let t = StringMap.find_opt a map in
                          match t with
                          |Some(TypA(b,l)) -> Ival(b)
                          |Some(TypI) -> failwith (Printf.sprintf "Invalid type for base: %s is int" a)
                          |None -> failwith (Printf.sprintf "Invalid type for base: %s is unknown" a))

(** Transient variable map implementation
      @param is   instruction list
      @param is1  instruction list accumulator
      @param rho  environment
      @param n    the counter for the updates of rho
      @return ((is1,i,is2),rho)  the splitted instruction list and the updated environment
*)
let rec trvar_map is is1 rho n =
  match (n,is) with
  | (1, x::xs) -> ((is1, x, xs),rho)
  | (n, x::xs) when n > 1 ->(
      match x with
      |AssignV(id,Ival(i))  -> trvar_map xs (is1 @ [x]) (StringMap.add id i rho) (n-1) 
      |AssignE(id,e)        -> trvar_map xs (is1 @ [x]) (StringMap.remove id rho) (n-1)
      |Load(id,e)           -> trvar_map xs (is1 @ [x]) (StringMap.remove id rho) (n-1)
      |IProtectE(id,p,e)    -> trvar_map xs (is1 @ [x]) (StringMap.remove id rho) (n-1)
      |_                    -> trvar_map xs (is1 @ [x]) rho (n-1))
  | _ -> failwith "Invalid directive"

(** Predicate for instruction
      @param is       instruction
      @return boolean true if the instruction is a Store, false otherwise 
*)
let predStore is =
  match is with
  |StoreE(_,_) -> true
  |StoreV(_,_) -> true
  |_          -> false 

(** Predicate for instruction
      @param is       instruction
      @return boolean true if the instruction is a Guard, false otherwise 
*)
let predGuard is =
  match is with
  |Guard(_,_,_,_) -> true
  |_              -> false

(** Pending fail and guard identifiers creation.
      @param is  instruction list
      @return xs the list of identifiers 
*)
let rec pending is =
  match is with
  |[]                 -> []
  |Guard(_,_,_,p)::xs -> p :: pending xs
  |Fail(p)::xs        -> p::  pending xs
  |x::xs              -> pending xs

(**
   This function executes a fetch step (previously given as directive by the attacker) on the next command [c].
   This stage is executed according to a given rule system, described by the Blade paper.   
   @param conf the configuration
   @return the new configuration after a fetch step
*)
let eval_fetch (conf: configuration) = 
  if List.length conf.cs = 0 then failwith "Impossible to fetch: command list is empty"
  else
    let c = List.hd conf.cs in
    match c with
    (* FETCH-SKIP rule *)
    | Skip -> 
      (
        let ls = List.append conf.is [Nop] in
        let cs_t = List.tl conf.cs in
        {conf with is=ls ; cs=cs_t}
      ) 
    (* FETCH-FAIL rule *)
    | Fail -> 
      (
        let ls = List.append conf.is [Fail(fresh())] in
        let cs_t = List.tl conf.cs in
        {conf with is=ls ; cs=cs_t}
      )
    (* this type-constructor could correspond to more cases *)
    | VarAssign(id, rh) -> 
      (
        match rh with
        (* FETCH-ASGN rule *)
        | Expr(e) ->
          (
            let ls = List.append conf.is [AssignE(id, e)] in
            let cs_t = List.tl conf.cs in
            {conf with is=ls ; cs=cs_t}
          )
        (* FETCH-PTR-LOAD rule *)
        | PtrRead(e) ->
          (
            let ls = List.append conf.is [Load(id, e)] in
            let cs_t = List.tl conf.cs in
            {conf with is=ls ; cs=cs_t}
          )
        (* FETCH-ARRAY-LOAD rule *)
        | ArrayRead(id_arr, e) ->
          ( 
            let g = BinOp(e, Length(id_arr), "<") in
            let rhs_new = PtrRead(BinOp(Base(id_arr), e, "+")) in
            let asgn_new = VarAssign(id, rhs_new) in
            (* if(e < length(id_arr)) then id := ptr(base(id_arr) + e) else fail *)
            let cmd_new = If(g, asgn_new, Fail) in
            let cs_t = List.tl conf.cs in
            {conf with cs=cmd_new::cs_t}
          )
      )
    (* FETCH-PTR-STORE rule *)
    | PtrAssign(e1, e2) -> 
      (
        let ls = List.append conf.is [StoreE(e1, e2)] in
        let cs_t = List.tl conf.cs in
        {conf with is=ls ; cs=cs_t}  
      )
    (* FETCH-ARRAY-STORE rule *)
    | ArrAssign(id, e1, e2) -> 
      (
        let g = BinOp(e1, Length(id), "<") in
        let lhs_new = BinOp(Base(id), e1, "+") in
        let asgn_new = PtrAssign(lhs_new, e2) in
        (* if(e1 < length(id_arr)) then ptr(base(id_arr) + e1) := e2 else fail *)
        let cmd_new = If(g, asgn_new, Fail) in
        let cs_t = List.tl conf.cs in
        {conf with cs=cmd_new::cs_t}
      )
    (* FETCH-SEQ rule *)
    | Seq(c1, c2) -> 
      (
        let cs_t = List.tl conf.cs in
        {conf with cs=c1::c2::cs_t}
      )
    (* FETCH-WHILE rule *)
    | While(g, cm) -> 
      (
        let cs_t = List.tl conf.cs in
        let cm_seq = Seq(cm, c) in
        let w_unrolled = If(g, cm_seq, Skip) in
        {conf with cs=w_unrolled::cs_t}
      )
    (* this type-constructor could corresponde to more cases *)
    | Protect(id, pct, rhs) ->
      (
        match rhs with
        (* FETCH-PROTECT-EXPR rule *)
        | Expr(e) -> 
          (
            let ls = List.append conf.is [IProtectE(id, pct, e)] in 
            let cs_t = List.tl conf.cs in
            {conf with is=ls ; cs=cs_t}
          )
        (* FETCH-PROTECT-SLH rule *)
        | ArrayRead(id_arr, e) when pct=Slh ->
          (
            let e1 = BinOp(e, Length(id_arr), "<") in
            let e2 = BinOp(e, Base(id_arr), "+") in
            let tmp_var_name = make_fresh_var() in
            let c1 = VarAssign(tmp_var_name, Expr e1) in
            let all_ones = Int.lognot 0 in
            let c2 = VarAssign(tmp_var_name, Expr (BinOp(CstI all_ones, Var tmp_var_name, "*"))) in
            let c3 = VarAssign(id, PtrRead(BinOp(e2, Var tmp_var_name, "&"))) in
            let cmd_new = Seq(c1, If(Var tmp_var_name, Seq(c2, c3), Fail)) in
            let cs_t = List.tl conf.cs in
            {conf with cs=cmd_new::cs_t ; rho=(StringMap.add tmp_var_name 0 conf.rho)}
          ) 
        (* FETCH-PROTECT-ARRAY & FETCH-PROTECT-PTR (premises in the two rules are the same) *)
        | _ -> 
          (
            if pct=Slh then failwith "SLH protect method invalid for this expression"
            else 
              let id_new = "'"^id in
              let asgn_new = VarAssign(id_new, rhs) in 
              let protect_new = Protect(id, pct, Expr(Var(id_new))) in
              let cs_t = List.tl conf.cs in
              {conf with cs=asgn_new::protect_new::cs_t}
          )
      )
    | _ -> failwith "Invalid directive"

(** 
   This function execute a fetch step with branch predictor (previously given as directive by the attacker) on the next command [c]. 
   @param p the prediction done when fetching the if statement 
   @param conf the configuration  
   @return the new configuration after a fetch(p) step
*)
let eval_pfetch p conf = 
  let c = List.hd conf.cs in
  match c with
  | If(g, c1, c2) ->
    (
      let cs_t = List.tl conf.cs in
      if p then 
        (* FETCH-IF-TRUE rule *)
        let ls = List.append conf.is [Guard(g, p, c2::cs_t, fresh())] in
        {conf with is=ls ; cs=c1::cs_t}
      else 
        (* FETCH-IF-FALSE rule *)
        let ls = List.append conf.is [Guard(g, p, c1::cs_t, fresh())] in
        {conf with is=ls ; cs=c2::cs_t}
    )
  | _ -> failwith "Invalid directive"

(** Execute rule implementation
          @param n the n-th instruction to execute
          @param conf the configuration
          @param map map of declared variables
          @return (conf,obs) the resulting configuration and the observable created 
*)
let rec eval_exec n conf map=

  (** Execute stage
      @param is1  instructions before the one to execute
      @param is   instuction to execute
      @param is2  instructions after the one to execute
      @param cs   command list 
      @param rho' the updated environment
      @return (is,cs,obs)  the new instruction list, the new command list and the observable
  *) 
  let execute is1 is is2 cs rho' =
    match is with
    |AssignE(id,e)          -> (let v = eval_expr e rho' map in
                                let ilst =  (is1 @ [AssignV(id,v)] @ is2) in
                                (ilst, cs, None))
    |Load(id,e)             -> (if not (List.exists predStore is1) 
                                then match eval_expr e rho' map with
                                  |Ival(n) -> let ilst =  (is1 @ [AssignV(id,Ival(Array.get conf.mu n))] @ is2) in
                                    (ilst, cs, Read(n, pending is1))
                                  |_       -> failwith "Invalid type for array read" 
                                else failwith "Store in instructions before the load" )
    |StoreE(e1,e2)          -> (let n = eval_expr e1 rho' map in
                                let v = eval_expr e2 rho' map in
                                match (n,v) with
                                |(Ival(n'),Ival(v')) -> let ilst = (is1 @ [StoreV(n,v)] @ is2) in
                                  (ilst, cs, Write(n', pending is1))
                                |_                   -> failwith "Invalid type for store " 

                               )   
    |Guard(e,pred,cml,i)    -> (match eval_expr e rho' map with
        |Ival(b) -> if b == Bool.to_int(pred) 
          then (is1 @ [Nop] @ is2, cs, None) 
          else (is1 @ [Nop] , cml, Rollback(i))
        |_ -> failwith "Invalid type for guard")
    |IProtectV(id,p,v)      -> (if not (List.exists predGuard is1)
                                then let ilst =  (is1 @ [AssignV(id,v)] @ is2) in
                                  (ilst, cs, None)
                                else failwith "Guard in instructions before the protect" )
    |IProtectE(id,p,e)      -> (let v = eval_expr e rho' map in 
                                let ilst =  (is1 @ [IProtectV(id,p,v)] @ is2) in
                                (ilst, cs, None))
    |_                      -> failwith "Invalid directive"

  in 
  let ((is1, i, is2),rho) = trvar_map conf.is [] conf.rho n in
  let (ilst, cs, obs) = execute is1 i is2 conf.cs rho in
  ({is = ilst; cs = cs; mu = conf.mu;  rho = conf.rho}, obs)

(** Retire rule implementation
          @return (conf,obs) the resulting configuration and the observable created
*)
let eval_retire conf = 
  let istr = List.hd conf.is in
  match istr with
  | Nop                       -> ({ conf with is = List.tl conf.is }, None)
  | AssignV (id, Ival v)      -> (let rho' = StringMap.add id v conf.rho in
                                  let c' = {is = List.tl conf.is; cs = conf.cs; mu = conf.mu; rho = rho'} in
                                  (c', None))
  | StoreV (Ival v1, Ival v2) -> (Array.set conf.mu v1 v2;
                                  let c' = {is = List.tl conf.is; cs = conf.cs; mu = conf.mu; rho = conf.rho} in
                                  (c', None))
  | Fail (i)                  -> ({is = []; cs = []; mu = conf.mu; rho = conf.rho}, Fail(i))
  | _                         -> failwith "Invalid directive"


let speculator map f = 
  fun conf ->
  match (conf.is, conf.cs) with
  | (_, (If(e,c1,c2))::xs)  -> 
    (match eval_expr e conf.rho map with
     |Ival(b) -> if (b = 0) then PFetch(f false) else PFetch(f true) 
     |_       -> failwith "Invalid type for guard")
  | (Nop::is,_)
  | (AssignV(_,_)::is,_)
  | (StoreV(_,_)::is,_)
  | (Fail(_)::is,_)         -> Retire
  | (i::is, _)              -> Exec(1)
  | (_, x::xs)              -> Fetch
  |_ -> failwith "Invalid configuration"

let rec eval conf map attacker trace counter =
  match (conf.is, conf.cs) with
  | ([], []) -> (conf, trace, counter)
  | _ ->
    (
      let dir = attacker conf in

      match dir with
      | Fetch -> let conf = eval_fetch conf in 
        eval conf map attacker (trace @ [None]) (counter+1)
      | PFetch(p) -> let conf = eval_pfetch p conf in 
        eval conf map attacker (trace @ [None]) (counter+1)
      | Exec(n) -> let (conf, obs) = eval_exec n conf map in
        eval conf map attacker (trace @ [obs]) (counter+1)
      | Retire -> let (conf, obs) = eval_retire conf in 
        eval conf map attacker (trace @ [obs]) (counter+1)
    )