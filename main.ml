open Ast

open Opal
open Parser
open Blade
open Util
open Eval

let _ =
  let src = LazyStream.of_channel stdin in
  match Parser.parse src with
  | None -> failwith "Syntax error"
  | Some c -> Printf.printf "%s\n\n\n" (print_cmd c);
    let c' = blade c in
    Printf.printf "%s\n" (print_cmd c')
