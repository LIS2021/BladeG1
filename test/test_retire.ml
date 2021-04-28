open Ast
open Eval
open Parser
open Util
open Vm_types

let attacker_retire conf = Retire

let print_conf conf = 
  List.iter (fun i -> print_string (print_istr i)) conf.is;
  List.map Util.print_cmd conf.cs |> ignore;
  Array.iter (fun i -> Printf.printf "%d " i) conf.mu;
  print_string (print_rho conf.rho);
  Printf.printf "\n"

let test id_test (conf: configuration) = 
  let (map, command) = parse_string_fail parse_decls_cmd "
  a: int;
  a := 1;" in
  Printf.printf id_test;
  Printf.printf "Before: \n";
  print_conf conf;

  let (c, obs, counter) = eval conf map attacker_retire [] 0 in
  Printf.printf "After: \n";
  print_conf c;
  Printf.printf "Obs: ";
  print_string(print_obs (List.hd obs));
  Printf.printf "\n"

let test_nop = 
  let conf = {
    is = [Nop]; 
    cs = []; 
    mu = [|1;2;3|]; 
    rho = StringMap.empty;
  } 
  in 
  test "--- Test Nop --- \n" conf

let test_assignV = 
  let conf = {
    is = [AssignV("x", Ival(1))]; 
    cs = []; 
    mu = [|1;2;3|]; 
    rho = StringMap.empty;
  } 
  in 
  test "--- Test AssignV --- \n" conf

let test_storeV = 
  let conf = {
    is = [StoreV(Ival(1), Ival(1))]; 
    cs = []; 
    mu = [|1;2;3|];
    rho = StringMap.empty;
  } 
  in 
  test "--- Test StoreV --- \n" conf

let test_fail = 
  let conf = {
    is = [Fail(1)]; 
    cs = []; 
    mu = [|1;2;3|]; 
    rho = StringMap.empty;
  } 
  in 
  test "--- Test Fail --- \n" conf

let test_oths = 
  let conf = {
    is = [Load("x", Var("y"))];
    cs = []; 
    mu = [||]; 
    rho = StringMap.empty;
  } 
  in 
  test "--- Test Other Istr --- \n" conf