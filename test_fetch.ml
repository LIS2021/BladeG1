open Ast;;
open Eval;;
open Parser;;
open Util;;

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

let test1 =
  let (decs, command) = parse_string_fail parse_decls_cmd "
  i1: int;
  i2: int;
  x: int;
  b: int;
  y: int;
  z: int;
  w: int;
  a: [0, 10];
  c: [0, 10];

  i1 := 2;
  i2 := 4;
  a[i1] := 7;
  x := a[i1];
  if b then
    y := a[i2]
  else
    fail
  endif;
  z := x + y;
  w := c[z]
  " in

  let conf = {
    is = [];
    cs = [command];
    mu = [||];
    rho = StringMap.empty;
  }
  in eval conf decs fetch_lover_attacker [] 0 

let test2 =
  let (decs, command) = parse_string_fail parse_decls_cmd "
  i1: int;
  i2: int;
  x: int;
  b: int;
  y: int;
  z: int;
  w: int;
  a: [0, 10];
  c: [0, 10];

  i1 := 2;
  i2 := 4;
  x := a[i1];
  while b do
    y := a[i2]
  endwhile;
  z := x + y;
  w := c[z]
  " in
  let conf = {
    is = [];
    cs = [command];
    mu = [||];
    rho = StringMap.empty;
  }
  in eval conf decs fetch_lover_attacker [] 0 

let test3 =
  let (decs, command) = parse_string_fail parse_decls_cmd "
  i1: int;
  i2: int;
  x: int;
  b: int;
  y: int;
  z: int;
  w: int;
  a: [0, 10];
  c: [0, 10];

  i1 := 2;
  i2 := 4;
  i1 := *x;
  while b do
    y := a[i2]
  endwhile;
  z := x + y;
  w := c[z]
  " in

  let conf = {
    is = [];
    cs = [command];
    mu = [||];
    rho = StringMap.empty;
  }
  in eval conf decs fetch_lover_attacker [] 0 

(* TODO? Con la sintassi attuale non possiamo esprimere *x = a[i] *)
let _ = test3

