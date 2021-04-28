open Ast;;
open Eval;;
open Parser;;
open Util;;
open Vm_types;;

let fetch_lover_attacker conf = 
  try
    let c = List.hd conf.cs in
    match c with
    | If(_, _, _) -> PFetch (Random.bool())
    | _ -> Fetch
  with
  | _ -> 
    (let _ = List.iter (fun is -> print_string( print_istr is)) conf.is in
     Fetch)

let test1() =
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

let test2() =
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

let test3() =
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

(* this is just for testing, ignoring fails is a bad practice *)
let eval_step_trying test =
  try
    let _ = test() in ()
  with
  | _ -> let _ = Printf.printf "_______________________\n" in ()

let _ = 
  let _ = eval_step_trying test1 in
  let _ = eval_step_trying test2 in 
  eval_step_trying test3;

