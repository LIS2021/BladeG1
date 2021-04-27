open Ast
open Parser
open Vm_types
open Eval
open Util

let spec_type = ref "perfect"
let hit_rate = ref 0.95

let options = [("-t", Arg.Set_string spec_type, "Speculator type. Can be 'perfect', 'worst' or 'probabilistic' (default: 'perfect')");
               ("--hit-rate", Arg.Set_float hit_rate, "Probabilistic speculator hit rate (default: 0.95)")]

let _ =
  Arg.parse options (fun _ -> ()) "Runs a While program in a speculative virtual machine";
  let f = match !spec_type with
    | "perfect" -> (fun x -> x)
    | "worst" -> Bool.not
    | "probabilistic" -> (fun x -> if (Random.float 1.) > !hit_rate then not x else x)
    | _ -> failwith "Invalid speculator type"
  in let (decls, c) = parse_channel_fail parse_decls_cmd stdin in
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
  let spec = speculator decls f in
  let (conf', oblist, _) = eval conf decls spec [] 0 in
  print_rho conf'.rho;
  print_mu conf'.mu;
  List.iter (fun o -> print_obs o) oblist;