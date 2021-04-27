open Ast
open Eval

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
  | Nop -> print_string "nop;\n"
  | AssignE(id, e) -> print_string (id^" := "^(print_expr e)^";\n")
  | AssignV(id, Ival(i)) -> print_string(id^" <- "^ string_of_int i^";\n")
  | Load(id, e) -> print_string (id^" := load("^(print_expr e)^");\n")
  | StoreE(e1, e2)-> print_string ((print_expr e1)^" := store("^(print_expr e2)^");\n")
  | StoreV(Ival(x), Ival(y))-> print_string ("mu["^string_of_int x ^"] := "^(string_of_int y)^");\n")
  | IProtectE(id, prt, e) -> print_string (id^"protect("^(print_expr e)^");\n")
  | IProtectV(id, prt, Ival(x)) -> print_string (id^"protect("^(string_of_int(x))^");\n")
  | Guard(e, p, lst, n) -> print_string ("guard("^(print_expr e)^", "^(string_of_bool p)^", "^(string_of_int n)^");\n")
  | Fail(n) -> print_string ("fail("^(string_of_int n)^");\n")
  | _ -> print_string ""

(** Returns a string representation of the rho memory
    @param rho
    @return string
*)
let print_rho rho = StringMap.iter (fun x y -> print_string(x^" := "^ string_of_int(y)^"\n")) rho 

(** Returns a string representation of an observable
    @param o
    @return string
*)
let print_obs o = match o with
  | None                    -> print_string("None\n")
  | Read(i,lst)             -> Printf.printf "Read %d, [" i;
    List.iter (fun i ->Printf.printf "%d " i) lst;
    Printf.printf "]\n"
  | Write(i,lst)            -> Printf.printf "Write %d, [" i;
    List.iter (fun i ->Printf.printf "%d " i) lst;
    Printf.printf "]\n"
  | Fail(i)                 -> Printf.printf "Fail %d \n" i
  | Rollback(i)             -> Printf.printf "Rollback %d \n" i;;