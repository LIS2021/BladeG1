open Ast;;
open Eval;;
open Parser;;
open Util;;

let attacker (conf:configuration) = Exec(1);;

(** Test trvar_map *)
let test1 = 
  let rho = ( let r1 = StringMap.add "x" 1 StringMap.empty in
              let r2 = StringMap.add "y" 1 r1 in
              let r3 = StringMap.add "z" 1 r2 in
              let r4 = StringMap.add "u" 1 r3 in
              StringMap.add "w" 1 r4 ) in 
  let is = [AssignV("a",Ival(42)); AssignE("y", Var("z")); Load("z", CstI(1)); IProtectE("u", Slh, Var("x")); AssignV("w",Ival(42))] in
  let ((is1, i, is2), rho') = trvar_map is [] rho 5 in
  print_string("-------- TEST1 --------\n");
  print_string("Instruction list:\n");
  List.iter print_istr is;
  print_string("Rho before transient variable map:\n");
  print_rho(rho);
  print_string("Rho after transient variable map:\n");
  print_rho(rho');
  print_string("-----------------------\n");;
  

(** Test pending  *)
let test2 = 
  let is = [AssignV("a",Ival(42)); Fail(42); Load("z", CstI(1)); Guard(CstI(1),true,[Skip], 0)] in
  let p = pending is in
  print_string("-------- TEST2 --------\n");
  print_string("Instruction list:\n");
  List.iter print_istr is;
  print_string("Pending List:\n");
  List.iter (fun i ->Printf.printf "%d\n" i) p;
  print_string("-----------------------\n");;

let map = let m1 = StringMap.add "a" TypI StringMap.empty in
  let m2 = StringMap.add "b" TypI m1 in
  let m3 = StringMap.add "c" (TypA(0,10)) m2 in
  let m4 = StringMap.add "x" TypI m3 in
  StringMap.add "y" TypI m4 ;;

let conf1 = {
  is=[AssignE("a", CstI(42));
      Load("b", CstI(1));
      StoreE(CstI(4), CstI(8));
      Load("b", CstI(1));
      IProtectV("x", Fence, Ival(1));
      Guard(CstI(0),false, [Skip], 1);
      Guard(CstI(0),true, [Skip], 2);
      IProtectV("x", Fence, Ival(1));
      IProtectE("a", Fence, Var("y"));
      Fail(3)]; 
  cs=[]; 
  mu=[|0;42;0;0;0;0;0;0;0;0|]; 
  rho= StringMap.add "y" 8 StringMap.empty
};;
(** Test execute on AssignE *)
let test3 = 
  let  (conf,obs) = eval_exec 1 conf1 map in
  print_string("-------- TEST3 --------\n");
  print_string("Instruction list before execute:\n");
  List.iter print_istr conf1.is;
  print_string("Instruction list after execute:\n");
  List.iter print_istr conf.is;
  print_string ("Observable: ");
  print_obs(obs);
  print_string("-----------------------\n");;

(** Test execute on Load *)
let test4 =
  let  (conf,obs) = eval_exec 2 conf1 map in
  print_string("-------- TEST4 --------\n");
  print_string("Instruction list before execute:\n");
  List.iter print_istr conf1.is;
  print_string("Instruction list after execute:\n");
  List.iter print_istr conf.is;
  print_string ("Observable: ");
  print_obs(obs);
  print_string("-----------------------\n");;

(** Test execute on StoreE *)
let test5 =
  let  (conf,obs) = eval_exec 3 conf1 map in
  print_string("-------- TEST5 --------\n");
  print_string("Instruction list before execute:\n");
  List.iter print_istr conf1.is;
  print_string("Instruction list after execute:\n");
  List.iter print_istr conf.is;
  print_string ("Observable: ");
  print_obs(obs);
  print_string("-----------------------\n");;

(** Test execute on Load, failure case *)
let test6 = 
  try( ignore (eval_exec 4 conf1 map)  )
  with Failure m -> 
    print_string("-------- TEST6 --------\n");
    print_string("Error message:\n");
    print_string( m );
    print_string("\n-----------------------\n");;

(** Test execute on IProtectV *)
let test7 =
  let  (conf,obs) = eval_exec 5 conf1 map in
  print_string("-------- TEST7 --------\n");
  print_string("Instruction list before execute:\n");
  List.iter print_istr conf1.is;
  print_string("Instruction list after execute:\n");
  List.iter print_istr conf.is;
  print_string ("Observable: ");
  print_obs(obs);
  print_string("-----------------------\n");;

(** Test execute on Guard *)
let test8 =
  let  (conf,obs) = eval_exec 6 conf1 map in
  print_string("-------- TEST8 --------\n");
  print_string("Instruction list before execute:\n");
  List.iter print_istr conf1.is;
  print_string("Instruction list after execute:\n");
  List.iter print_istr conf.is;
  print_string ("Observable: ");
  print_obs(obs);
  print_string("-----------------------\n");;

(** Test execute on Guard, misprediction *)
let test9 =
  let  (conf,obs) = eval_exec 7 conf1 map in
  print_string("-------- TEST9 --------\n");
  print_string("Instruction list before execute:\n");
  List.iter print_istr conf1.is;
  print_string("Instruction list after execute:\n");
  List.iter print_istr conf.is;
  print_string ("Observable: ");
  print_obs(obs);
  print_string("-----------------------\n");;

(**Test execute on IProtectV, failure case *)
let test10 = 
try( ignore (eval_exec 8 conf1 map)  )
  with Failure m -> 
    print_string("-------- TEST10 --------\n");
    print_string("Error message:\n");
    print_string( m );
    print_string("\n-----------------------\n");;

(** Test execute on IprotectE *)
let test11 =
  let  (conf,obs) = eval_exec 9 conf1 map in
  print_string("-------- TEST11 --------\n");
  print_string("Instruction list before execute:\n");
  List.iter print_istr conf1.is;
  print_string("Instruction list after execute:\n");
  List.iter print_istr conf.is;
  print_string ("Observable: ");
  print_obs(obs);
  print_string("-----------------------\n");;

(** Test execute on IprotectE *)
let test12 =
  try( ignore (eval_exec 10 conf1 map)  )
  with Failure m -> 
    print_string("-------- TEST10 --------\n");
    print_string("Error message:\n");
    print_string( m );
    print_string("\n-----------------------\n");;
