(** Runs a While program in a naive interpreter *)

open Ast
open Parser

module StringMap = Map.Make(String)

let get_array_bounds (decls: decl_type StringMap.t) (name: identifier) =
  match StringMap.find name decls with
  | TypA(b, l) -> (b,l)
  | _ -> failwith "Invalid access"

let rec eval_expr (decls: decl_type StringMap.t) (mem: int Array.t) (store: (string, int) Hashtbl.t) e =
  let eval_expr = eval_expr decls mem store in
  match e with
  | CstI x -> x
  | Var v -> Hashtbl.find store v
  | BinOp(e1, e2, op) ->
    let v1 = eval_expr e1 in
    let v2 = eval_expr e2 in
    (match op with
     | "+" -> v1 + v2
     | "-" -> v1 - v2
     | "*" -> v1 * v2
     | "<" -> Bool.to_int (v1 < v2)
     | _ -> failwith "Not implemented")
  | InlineIf(e1, e2, e3) ->
    let cond = eval_expr e1 in
    eval_expr (if cond = 0 then e3 else e2)
  | Length n ->
    let (_, l) = get_array_bounds decls n in
    l
  | Base n ->
    let (b, _) = get_array_bounds decls n in
    b

let eval_rhs decls mem store r =
  let eval_expr = eval_expr decls mem store in
  match r with
  | Expr e -> eval_expr e
  | PtrRead e ->
    let ptr = eval_expr e in
    mem.(ptr)
  | ArrayRead (v, e) ->
    let (b, l) = get_array_bounds decls v in
    let idx = eval_expr e in
    if idx < l then
      mem.(b + idx) 
    else
      failwith "Out of bounds access"

let rec eval_cmd decls mem store c =
  let eval_expr = eval_expr decls mem store in
  let eval_rhs = eval_rhs decls mem store in
  match c with
  | Skip -> (mem, store)
  | Fail -> failwith "Failure"
  | VarAssign(n, r) ->
    let v = eval_rhs r in
    Hashtbl.replace store n v;
    (mem, store)
  | PtrAssign(e1, e2) ->
    let ptr = eval_expr e1 in
    let v = eval_expr e2 in
    mem.(ptr) <- v;
    (mem, store)
  | ArrAssign(n, e1, e2) ->
    let (b, l) = get_array_bounds decls n in
    let idx = eval_expr e1 in
    let v = eval_expr e2 in
    (if idx < l then (mem.(b + idx) <- v;) else failwith "Out of bounds access");
    (mem, store)
  | Seq(c1, c2) ->
    let (mem, store) = eval_cmd decls mem store c1 in
    let (mem, store) = eval_cmd decls mem store c2 in
    (mem, store)
  | If(e, c1, c2) ->
    let v = eval_expr e in
    eval_cmd decls mem store (if v = 0 then c2 else c1)
  | While(e, c1) ->
    let v = eval_expr e in
    if v = 0 then
      (mem, store)
    else
      let (mem, store) = eval_cmd decls mem store c1 in
      eval_cmd decls mem store c
  | Protect(n, _, r) ->
    let v = eval_rhs r in
    Hashtbl.replace store n v;
    (mem, store)

let print_store store =
  Hashtbl.iter (fun k v -> Printf.printf "%s: %d\n" k v) store

let print_mem mem =
  print_string "memory: [";
  Array.iter (fun v -> Printf.printf "%d, " v) mem;
  print_string "]\n"

let _ =
  let (decls, c) = parse_channel_fail parse_decls_cmd stdin in
  let array_max = StringMap.fold (fun _ v vs -> match v with
      | TypA(b, l) -> (b+l)::vs
      | _ -> vs) decls [] in
  let mem_size = List.fold_left max 0 array_max in
  let mem: int Array.t = Array.make mem_size 0 in 
  let store = StringMap.cardinal decls |> Hashtbl.create in
  StringMap.fold (fun k v _ -> if v = TypI then Hashtbl.replace store k 0 else ()) decls ();
  let (mem, store) = eval_cmd decls mem store c in
  print_store store;
  print_mem mem