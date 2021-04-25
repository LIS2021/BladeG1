open Ast;;
open Eval;;

let rec print_expr e =
  match e with
  | CstI num -> string_of_int num
  | Var id -> id
  | BinOp(e1, e2, op) -> (print_expr e1)^op^(print_expr e2)
  | InlineIf(e1, e2, e3) -> (print_expr e1)^"? "^(print_expr e2)^" : "^(print_expr e3)
  | Length id -> "length("^id^")"
  | Base id -> "base("^id^")"

let print_value v =
  match v with
  | Ival num -> string_of_int num^";\n"
  | Aval(b,m) -> ";\n"
  | Pval num -> string_of_int num^";\n"

let print_istr istr =
  match istr with
  | Nop -> print_string "nop;\n"
  | AssignE(id, e) -> print_string (id^" := "^(print_expr e)^";\n")
  | Load(id, e) -> print_string (id^" := load("^(print_expr e)^");\n")
  | StoreE(e1, e2)-> print_string ((print_expr e1)^" := store("^(print_expr e2)^");\n")
  | IProtectE(id, prt, e) -> print_string (id^"protect("^(print_expr e)^");\n")
  | Guard(e, p, lst, n) -> print_string ("guard("^(print_expr e)^", "^(string_of_bool p)^", "^(string_of_int n)^");\n")
  | Fail(n) -> print_string ("fail("^(string_of_int n)^");\n")
  | _ -> print_string ""


let fetch_lover_attacker conf = 
  try
    let c = List.hd conf.cs in
    match c with
    | If(_, _, _) -> PFetch (Random.bool())
    | _ -> Fetch
  with
  | _ -> 
    List.iter (fun is -> print_istr is) conf.is;
    failwith "Unknown"


(*
   i1 := 2
   i2 := 4
   a[i1] := 7
   x := a[i1]
   if b then y := a[i2] then fail
   z := x + y
   w := c[z]
*)
let test1 =
  let decs =
    let map = StringMap.empty in
    let _ = StringMap.add "i1" TypI map in
    let _ = StringMap.add "i2" TypI map in
    let _ = StringMap.add "x" TypI map in
    let _ = StringMap.add "b" TypI map in
    let _ = StringMap.add "y" TypI map in
    let _ = StringMap.add "z" TypI map in
    let _ = StringMap.add "w" TypI map in
    let _ = StringMap.add "a" (TypA(0, 10)) map in
    StringMap.add "c" (TypA(0, 10)) map
  in 

  let command = [
    Seq(VarAssign("i1",Expr(CstI(2))),
        Seq(VarAssign("i2",Expr(CstI(4))),
            Seq( ArrAssign("a", Var("i1"), CstI(7)),
                 Seq(VarAssign("x", ArrayRead("a",Var("i1"))),
                     Seq(If(Var("b"), VarAssign("y", ArrayRead("a", Var("i2"))), Fail), 
                         Seq(VarAssign("z", Expr(BinOp(Var("x"), Var("y"), "+"))),
                             VarAssign("w", ArrayRead("c", Var("z"))))
                        )
                    )
               )
           )
       )  
  ]
  in

  let conf = {
    is = [];
    cs = command;
    mu = [||];
    rho = StringMap.empty;
  }
  in eval conf decs fetch_lover_attacker [] 0 

  (*
   i1 := 2
   i2 := 4
   x := a[i1]
   while b 
        y := a[i2] 
   endwhile
   z := x + y
   w := c[z]
 *)
let test2 =
  let decs =
    let map = StringMap.empty in
    let _ = StringMap.add "i1" TypI map in
    let _ = StringMap.add "i2" TypI map in
    let _ = StringMap.add "x" TypI map in
    let _ = StringMap.add "b" TypI map in
    let _ = StringMap.add "y" TypI map in
    let _ = StringMap.add "z" TypI map in
    let _ = StringMap.add "w" TypI map in
    let _ = StringMap.add "a" (TypA(0, 10)) map in
    StringMap.add "c" (TypA(0, 10)) map
  in 

  let command = [
    Seq(VarAssign("i1",Expr(CstI(2))),
        Seq(VarAssign("i2",Expr(CstI(4))),
            Seq(VarAssign("x", ArrayRead("a",Var("i1"))),
                Seq(While(Var("b"), VarAssign("y", ArrayRead("a", Var("i2")))), 
                    Seq(VarAssign("z", Expr(BinOp(Var("x"), Var("y"), "+"))),
                        VarAssign("w", ArrayRead("c", Var("z"))))
                   )
               )
           )
       )  
  ]
  in

  let conf = {
    is = [];
    cs = command;
    mu = [||];
    rho = StringMap.empty;
  }
  in eval conf decs fetch_lover_attacker [] 0 

(*
   i1 := 2
   i2 := 4
   i1 := *x
   while b 
        y := a[i2] 
   endwhile
   z := x + y
   w := c[z]
*)
let test3 =
  let decs =
    let map = StringMap.empty in
    let _ = StringMap.add "i1" TypI map in
    let _ = StringMap.add "i2" TypI map in
    let _ = StringMap.add "x" TypP map in
    let _ = StringMap.add "b" TypI map in
    let _ = StringMap.add "y" TypI map in
    let _ = StringMap.add "z" TypI map in
    let _ = StringMap.add "w" TypI map in
    let _ = StringMap.add "a" (TypA(0, 10)) map in
    StringMap.add "c" (TypA(0, 10)) map
  in 

  let command = [
    Seq(VarAssign("i1",Expr(CstI(2))),
        Seq(VarAssign("i2",Expr(CstI(4))),
            Seq(VarAssign("i1", PtrRead(Var "x")),
                Seq(While(Var("b"), VarAssign("y", ArrayRead("a", Var("i2")))), 
                    Seq(VarAssign("z", Expr(BinOp(Var("x"), Var("y"), "+"))),
                        VarAssign("w", ArrayRead("c", Var("z"))))
                   )
               )
           )
       )  
  ]
  in

  let conf = {
    is = [];
    cs = command;
    mu = [||];
    rho = StringMap.empty;
  }
  in eval conf decs fetch_lover_attacker [] 0 

(* TODO? Con la sintassi attuale non possiamo esprimere *x = a[i] *)
let _ = test3

