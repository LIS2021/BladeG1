open Ast
open Parser
open Vm_types
open Eval
open Util

let _ =
  let (decls, c) = parse_channel_fail parse_decls_cmd stdin in
  let array_max = StringMap.fold (fun _ v vs -> match v with
      | TypA(b, l) -> (b+l)::vs
      | _ -> vs) decls [] in
  let mem_size = List.fold_left max 0 array_max in
  let mem: int Array.t = Array.make mem_size 0 in 
  let store = StringMap.fold (fun k v s -> if v = TypI then StringMap.add k 0 s else s) decls StringMap.empty
  in let conf = {
      is = []; 
      cs = [c] ; 
      mu = mem ; 
      rho = store ;
    } in
  let attacker = speculator decls Bool.not in
  let spec = speculator decls (fun x -> x) in
  let (conf', oblist, _) = eval conf decls spec [] 0 in
  print_rho conf'.rho;
  print_mu conf'.mu;
  List.iter (fun o -> print_obs o) oblist;