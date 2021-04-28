open Ast
open Vm_types

module StringMap = Map.Make(String)

(** Returns a string representation of an expression. 
    @param e the expression
    @return string
*)
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

(** Returns a string representation of the right-hand side of an assignment. 
    @param r the rhs of an assignment
    @return string
*)
let print_rhs (r: rhs) : string = match r with
  | Expr e -> print_expr e
  | PtrRead e -> Printf.sprintf "*(%s)" (print_expr e)
  | ArrayRead (i, e) -> Printf.sprintf "%s[%s]" i (print_expr e)

(** Returns a string representation of the right-hand side of a command. 
    @param c the command
    @return string
*)
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

(** Returns a string representation of the declarations' map
    @param map
    @return string
*)
let print_decls (map: decl_type StringMap.t) : string =
  let print_type t = match t with
    | TypA (b, l) -> Printf.sprintf "[%d, %d]" b l
    | TypI -> "int"
  in StringMap.fold (fun k t s -> Printf.sprintf "%s: %s;\n%s" k (print_type t) s) map ""

(** Returns a string representation of a value
    @param v 
    @return string
*)
let print_value v =
  match v with
  | Ival num -> string_of_int num^";\n"
  | Aval(b,m) -> "{base: "^(string_of_int b)^"; length: "^(string_of_int m)^";\n"
  | Pval num -> string_of_int num^";\n"

(** Returns a string representation of an instruction
    @param istr
    @return string
*)
let print_istr istr =
  match istr with
  | Nop                         -> "nop;\n"
  | AssignE(id, e)              -> id^" := "^(print_expr e)^";\n"
  | AssignV(id, Ival(i))        -> id^" <- "^ string_of_int i^";\n"
  | Load(id, e)                 -> id^" := load("^(print_expr e)^");\n"
  | StoreE(e1, e2)              -> (print_expr e1)^" := store("^(print_expr e2)^");\n"
  | StoreV(Ival(x), Ival(y))    -> "mu["^string_of_int x ^"] <- "^(string_of_int y)^";\n"
  | IProtectE(id, prt, e)       -> id^" := protect("^(print_expr e)^");\n"
  | IProtectV(id, prt, Ival(x)) -> id^" <- protect("^(string_of_int(x))^");\n"
  | Guard(e, p, lst, n)         -> "guard("^(print_expr e)^", "^(string_of_bool p)^", "^(string_of_int n)^");\n"
  | Fail(n)                     -> "fail("^(string_of_int n)^");\n"
  | _                           -> ""

(** Returns a string representation of the rho memory
    @param rho
    @return string
*)
let print_rho rho = String.concat "" (StringMap.fold (fun x y l -> l @ [x^" := "^ string_of_int(y)^"\n"]) rho [] )

(** Returns a string representation of an observable
    @param o the observable
    @return string
*)
let print_obs o = match o with
  | None                    -> "None\n"
  | Read(i,lst)             -> let s0 = Printf.sprintf "Read %d, [" i in
                               let s1 = String.concat "" (List.map (fun i -> Printf.sprintf "%d " i) lst) in
                               let s2 = Printf.sprintf "]\n"in
                               s0^s1^s2
  | Write(i,lst)            -> let s0 = Printf.sprintf "Write %d, [" i in
                               let s1 = String.concat "" (List.map (fun i -> Printf.sprintf "%d " i) lst) in
                               let s2 = Printf.sprintf "]\n" in
                               s0^s1^s2
  | Fail(i)                 -> Printf.sprintf "Fail %d \n" i
  | Rollback(i)             -> Printf.sprintf "Rollback %d \n" i;;

let print_mu mu =
  let s0 = "memory: [" in
  let s1 = String.concat "" (Array.to_list(Array.map (fun v -> Printf.sprintf "%d, " v) mu)) in
  let s2 = "]\n" in
  s0^s1^s2;;