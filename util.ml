open Ast

module StringMap = Map.Make(String)

(** Returns a string representation of an expression. *)
let rec print_expr (e: expr) : string = match e with
  | CstI i -> Printf.sprintf "%d" i
  | Var n -> n
  | BinOp (e1, e2, op) -> Printf.sprintf "(%s %s %s)" (print_expr e1) op (print_expr e2)
  | InlineIf (e1, e2, e3) ->
    let s1 = print_expr e1 in
    let s2 = print_expr e2 in
    let s3 = print_expr e3 in Printf.sprintf "(%s ? %s : %s)" s1 s2 s3
  | Length i -> Printf.sprintf "length(%s)" i
  | Base i -> Printf.sprintf "base(%s)" i

(** Returns a string representation of the right-hand side of an assignment. *)
let print_rhs (r: rhs) : string = match r with
  | Expr e -> print_expr e
  | PtrRead e -> Printf.sprintf "*(%s)" (print_expr e)
  | ArrayRead (i, e) -> Printf.sprintf "%s[%s]" i (print_expr e)

(** Returns a string representation of the right-hand side of a command. *)
let print_cmd (c: cmd) : string =
  let rec helper_cmd (c: cmd) : string = match c with
    | Skip -> "skip"
    | Fail -> "fail"
    | VarAssign (i, r) -> Printf.sprintf "%s := %s" i (print_rhs r)
    | PtrAssign (e1, e2) -> Printf.sprintf "*(%s) := %s" (print_expr e1) (print_expr e2)
    | ArrAssign (i, e1, e2) -> Printf.sprintf "%s[%s] := %s" i (print_expr e1) (print_expr e2)
    | Seq (c1, c2) -> Printf.sprintf "%s;\n%s" (helper_cmd c1) (helper_cmd c2)
    | If (e, c1, c2) -> Printf.sprintf "if %s then\n%s\nelse\n%s\nendif" (print_expr e) (helper_cmd c1) (helper_cmd c2)
    | While (e, c) -> Printf.sprintf "while %s do\n%s\nendwhile" (print_expr e) (helper_cmd c)
    | Protect (i, p, r) ->
      let prot_type = match p with
        | Slh -> "protect_slh"
        | Fence -> "protect_fence"
        | Auto -> "protect"
      in Printf.sprintf "%s := %s(%s)" i prot_type (print_rhs r)
  in helper_cmd c

let print_decls (map: decl_type StringMap.t) : string =
  let print_type t = match t with
    | TypA (b, l) -> Printf.sprintf "[%d, %d]" b l
    | TypI -> "int"
  in StringMap.fold (fun k t s -> Printf.sprintf "%s: %s;\n%s" k (print_type t) s) map ""