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
  | StoreV of value * value
  | StoreE of expr * expr
  | IProtectV of identifier * protect * value
  | IProtectE of identifier * protect * expr     (*     id := protect(e)     *)
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
      (**TODO: - valutare il cambio del tipo della rho
         - valutare la necessità di un tipp puntatore
         - eval_expr, trvr_map,
         - in trvar_map iprotect
      *)
      let rec eval_exec n = 
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
                                  |("<", Ival(x), Ival(y)) -> Ival(Bool.to_int(x < y))
                                  |("*", Ival(x), Ival(y)) -> Ival(x land y)
                                  |_ -> failwith "Invalid type for binary operation")
          | InlineIf(e,e1,e2) -> (let e' = eval_expr e rho map in
                                  match e' with
                                  |Ival(b) -> if b == 1 then eval_expr e1 rho map else eval_expr e2 rho map
                                  |_       -> failwith "Invalid type for guard")
          | Length(a)         -> (let t = StringMap.find_opt a map in
                                  match t with
                                  |Some(TypA(b,l)) -> Ival(l)
                                  |_ -> failwith "Invalid type for lenght")
          | Base(a)           -> (let t = StringMap.find_opt a map in
                                  match t with
                                  |Some(TypA(b,l)) -> Ival(b)
                                  |_ -> failwith "Invalid type for base") 
        in
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
        in
        let predStore is =
          match is with
          |StoreE(_,_) -> true
          |StoreV(_,_) -> true
          |_          -> false 
        in
        let predGuard is =
          match is with
          |Guard(_,_,_,_) -> true
          |_              -> false
        in
        let rec pending is =
          match is with
          |[]                 -> []
          |Guard(_,_,_,p)::xs -> p :: pending xs
          |Fail(p)::xs        -> p::  pending xs
          |x::xs              -> pending xs
        in 
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
                                      else failwith "Invalid directive" )
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
                else (is1 @ [Nop] @ is2, cml, Rollback(i))
              |_ -> failwith "Invalid type for guard")
          |IProtectV(id,p,v)      -> (if not (List.exists predGuard is1)
                                      then let ilst =  (is1 @ [AssignV(id,v)] @ is2) in
                                        (ilst, cs, None)
                                      else failwith "Invalid directive" )
          |IProtectE(id,p,e)      -> (let v = eval_expr e rho' map in 
                                      let ilst =  (is1 @ [IProtectV(id,p,v)] @ is2) in
                                      (ilst, cs, None))
          |_                      -> failwith "Invalid directive"

        in 
        let ((is1, i, is2),rho) = trvar_map conf.is [] conf.rho n in
        let (ilst, cs, obs) = execute is1 i is2 conf.cs rho in
        eval {is = ilst; cs = cs; mu = conf.mu;  rho = conf.rho} map attacker (trace @ [obs]) (counter+1)
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
      | VarAssign (i, r) -> Printf.sprintf "%s := %s" i (helper_rhs r)
      | PtrAssign (e1, e2) -> Printf.sprintf "*(%s) := %s" (helper_expr e1) (helper_expr e2)
      | ArrAssign (i, e1, e2) -> Printf.sprintf "%s(%s) := %s" i (helper_expr e1) (helper_expr e2)
      | Seq (c1, c2) -> Printf.sprintf "%s;\n%s" (helper_cmd c1) (helper_cmd c2)
      | If (e, c1, c2) -> Printf.sprintf "if %s then\n%s\nelse\n%s\nendif" (helper_expr e) (helper_cmd c1) (helper_cmd c2)
      | While (e, c) -> Printf.sprintf "while %s do\n%s\nendwhile" (helper_expr e) (helper_cmd c)
      | Protect (i, p, r) ->
        let prot_type = match p with
          | Slh -> "protect_slh"
          | Fence -> "protect_fence"
          | Auto -> "protect"
        in Printf.sprintf "%s := %s(%s)" i prot_type (helper_rhs r)
  in helper_cmd c

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
         let g = G.connect g (varnode, rnode) 1 in
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
  in let rec repair_cmd (c: G.node cmd_node) : G.node cmd_node =
       match c with
       | SkipN -> SkipN
       | FailN -> FailN
       | VarAssignN (i, r, ni, nr) ->
         let ni_in_cut = List.mem ni min_cut in
         let nr_in_cut = List.mem nr min_cut in
         (match (ni_in_cut, nr_in_cut) with
          | false, true -> ProtectN (i, Auto, r)
          | true, false -> failwith "Invalid graph!"
          | _ -> VarAssignN(i, r, ni, nr))
       | PtrAssignN (e1, e2) -> PtrAssignN (e1, e2)
       | ArrAssignN (i, e1, e2) -> ArrAssignN (i, e1, e2)
       | SeqN (c1, c2) ->  SeqN (repair_cmd c1, repair_cmd c2)
       | IfN (e, c1, c2) -> IfN (e, repair_cmd c1, repair_cmd c2)
       | WhileN (e, c) -> WhileN (e, repair_cmd c)
       | ProtectN (i, p, r) -> ProtectN (i, p, r)

  in repair_cmd annotated_cmd |> strip_nodes

let _ =
  let c : cmd = (Seq(
      VarAssign("x", ArrayRead("a", Var("i"))),
      If(Var("x"), Skip, Skip))) in
  Printf.printf "%s\n\n" (print_cmd c);
  let c' = blade c in
  Printf.printf "%s\n" (print_cmd c')