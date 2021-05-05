module L = Llvm
module StringMap = Map.Make(String)

open Ast
open Parser
open Vm_types
open Util
open Arg

type vartype = Int of L.llvalue | Arr of int * int

let output_file = ref "a.bc"

(* LLVM global context *)
let llcontext = L.global_context ()

(* LLVM module *)
let module_m = L.create_module llcontext "WHILE"

(* Some useful LLVM IR type to use in the code generation *)
let int_type = L.i32_type llcontext
let void_type = L.void_type llcontext
let zero = L.const_int int_type 0

(* Table for mapping a binop type in the LLVM function that implements the corresponding instruction and its name (FOR INT LITERAL)*)
let primitive_bin_operators_int =
  [	"+",     (L.build_add, "add")
  ;	"-",     (L.build_sub, "sub")
  ;	"*",     (L.build_mul, "mult")
  ;	"/",     (L.build_sdiv, "div")
  ; 	"%",     (L.build_srem, "mod")
  ; 	"==",    (L.build_icmp L.Icmp.Eq, "equal")
  ; 	"!=", 	 (L.build_icmp L.Icmp.Ne, "neq")
  ; 	"<",     (L.build_icmp L.Icmp.Slt, "le")
  ; 	"<=", 	 (L.build_icmp L.Icmp.Sle, "leq")
  ; 	">",     (L.build_icmp L.Icmp.Sgt, "ge")
  ;   ">=",    (L.build_icmp L.Icmp.Sge, "geq")
  ;   "&", 	 (L.build_and, "and")
  ; 	"|", 	 (L.build_or, "or")
  ;   "^",     (L.build_xor, "xor")
  ]

(** Declare in the current module the print_float prototype
    	@param llvm_module the top-level container
*)  
let exit_declaration =
  let exit_t = L.function_type void_type [||] in
  let v = L.declare_function "fail" exit_t module_m in
  v

let printmu_declaration mem =
  let arr_type = L.type_of mem in
  let printmu_t = L.function_type void_type [| arr_type; int_type |] in
  let v = L.declare_function "print_mu" printmu_t module_m in
  v

let fundef = L.define_function "main" (L.function_type void_type [||]) module_m

(* Recursive function for build recursively lltype of llvalue *)
(*let rec ltype_of_typ ty = match ty with
  	| A.TypI 			-> int_type
  	| A.TypA(tp, x) 	-> L.array_type int_type x *)

(** Function for looking up for llvalue of a name in the current table or in the global table 
    	@param name the name for which we are searching for
    	@param tab the map associated to current scope
*)
let lookup name tab =
  try
    StringMap.find name tab
  with
  | _ -> L.lookup_global name module_m |> Option.get

(**
   		It adds a terminal instruction if there isn't one already
   		@param builder 	the builder used to generate instructions
   		@param f 		A partial application of the function associated to the terminal instruction we have to generate, if there isn't one already
   	*)
let add_terminal builder f =
  match L.block_terminator (L.insertion_block builder) with
  | Some _ -> ()
  | None -> let _ = f builder in () 

let rec build_expr e tab mem ibuilder =
  match e with
  |CstI(i) 			-> 	(L.const_int int_type i, ibuilder)
  |Var(id)			-> 	(match StringMap.find id tab with
      |Int(li) -> (L.build_load li id ibuilder, ibuilder)
      |Arr(b,l) -> failwith "Invalid evaluation")
  |BinOp(e1, e2, op) 	->
    let le1, ibuilder = build_expr e1 tab mem ibuilder in
    let le2, ibuilder = build_expr e2 tab mem ibuilder in
    let (lop, lab) = List.assoc op primitive_bin_operators_int in
    let le3 = lop le1 le2 lab ibuilder in
    (L.build_intcast le3 int_type "ext" ibuilder, ibuilder)
  |InlineIf(e, e1, e2)->
    (
      let bool_val, ibuilder = build_expr e tab mem ibuilder in
      let bthen = L.append_block llcontext "then" fundef in
      let belse = L.append_block llcontext "else" fundef in
      let bcont = L.append_block llcontext "cont" fundef in
      let then_builder = L.builder_at_end llcontext bthen in
      let v1, bt = build_expr e1 tab mem then_builder in 
      let _ = add_terminal bt (L.build_br bcont) in 
      let else_builder = L.builder_at_end llcontext belse in
      let v2, be = build_expr e2 tab mem else_builder in
      let _ = add_terminal be (L.build_br bcont) in
      let _ = L.build_cond_br bool_val bthen belse ibuilder in
      let _ = L.position_at_end bcont ibuilder in
      let phi = L.build_phi [(v1, bthen); (v2, belse)] "phi" ibuilder in
      (phi, L.builder_at_end llcontext bcont)
    )
  |Length(id)			->	(match StringMap.find id tab with
      |Arr(b,l) -> (L.const_int int_type l, ibuilder)
      |_ -> failwith "Invalid evaluation")
  |Base(id)			-> 	(match StringMap.find id tab with
      |Arr(b,l) -> (L.const_int int_type b, ibuilder)
      |_ -> failwith "Invalid evaluation")

let rec build_command c tab mem ibuilder =
  let fail_func = L.lookup_function "fail" module_m |> Option.get in
  let rec build_rhs rh tab mem ibuilder =
    match rh with
    |Expr(expr) 		-> build_expr expr tab mem ibuilder
    |PtrRead(expr)		->
      let le, ibuilder = build_expr expr tab mem ibuilder in
      let gep = L.build_gep mem [|le|] "ptr" ibuilder in
      let load = L.build_load gep "load" ibuilder in
      (load, ibuilder)
    |_					-> failwith "Invalid option" 
  in
  match c with
  | Skip -> ibuilder 
  | Fail -> let _ = L.build_call fail_func [||] "" ibuilder in ibuilder
  | VarAssign(id, rh) ->
    ( match rh with
      | ArrayRead(ide,expr)	->
        let g = BinOp(expr, Length(ide), "<") in
        let rhs_new = PtrRead(BinOp(Base(ide), expr, "+")) in
        let asgn_new = VarAssign(id, rhs_new) in
        let cmd_new = If(g, asgn_new, Fail) in
        build_command cmd_new tab mem ibuilder
      | _					-> (let rh_value, ibuilder = build_rhs rh tab mem ibuilder in
                  let value = StringMap.find id tab in
                  match value with
                  | Int(lvalue) -> let _ = L.build_store rh_value lvalue ibuilder in ibuilder 
                  | Arr(_,_) -> failwith "Invalid assignment!")
    ) 
  | PtrAssign(e1, e2) -> 
    (
      let v1, ibuilder = build_expr e1 tab mem ibuilder in
      let v2, ibuilder = build_expr e2 tab mem ibuilder in
      let gep = L.build_gep mem [| v1 |] "ptr" ibuilder in
      let _ = L.build_store v2 gep ibuilder in
      ibuilder
    )
  | ArrAssign(id, idx, e) ->
    (
      let g = BinOp(idx, Length(id), "<") in
      let lh = BinOp(Base(id), idx, "+") in
      let length_check = If(g, PtrAssign(lh,e), Fail) in
      build_command length_check tab mem ibuilder
    )
  | Seq(c1, c2) ->
    (
      let builder' = build_command c1 tab mem ibuilder in
      build_command c2 tab mem builder'   
    )
  | If(g, c1, c2) ->
    (
      let cond_val, ibuilder = build_expr g tab mem ibuilder in
      let bool_val = L.build_icmp L.Icmp.Ne zero cond_val "if_cond" ibuilder in
      let bthen = L.append_block llcontext "then" fundef in
      let belse = L.append_block llcontext "else" fundef in
      let bcont = L.append_block llcontext "cont" fundef in
      let then_builder = L.builder_at_end llcontext bthen in
      let bt = build_command c1 tab mem then_builder in 
      let _ = add_terminal bt (L.build_br bcont) in 
      let else_builder = L.builder_at_end llcontext belse in
      let be = build_command c2 tab mem else_builder in
      let _ = add_terminal be (L.build_br bcont) in
      let _ = L.build_cond_br bool_val bthen belse ibuilder in
      let _ = L.position_at_end bcont ibuilder in
      L.builder_at_end llcontext bcont
    )
  | While(g, c) ->
    (
      let bexpr = L.append_block llcontext "while" fundef in
      let _ = L.build_br bexpr ibuilder in

      let bbody = L.append_block llcontext "while_body" fundef in
      let builder_body = L.builder_at_end llcontext bbody in 
      let bb = build_command c tab mem builder_body in
      let _ = add_terminal bb (L.build_br bexpr) in

      let builder_pred = L.builder_at_end llcontext bexpr in
      let cond_val , ibuilder= build_expr g tab mem builder_pred in
      let bool_val = L.build_icmp L.Icmp.Ne zero cond_val "while_cond" ibuilder in

      let bcont = L.append_block llcontext "while_cont" fundef in
      let _ = L.build_cond_br bool_val bbody bcont builder_pred in
      L.builder_at_end llcontext bcont
    )
  | Protect(id, Slh, ArrayRead(id_arr, e)) -> ibuilder
  (* TODO
     let zero = L.const_int int_type 0 in
     let one = L.build_not zero "not" ibuilder in
     let e_value = build_expr e tab mem ibuilder in
     let mask = L.build_mul one e_value "mask" ibuilder in

  *)
  | _ -> failwith "not yet implemented"

let options = [("-o", Arg.Set_string output_file, "Output file (default: 'a.bc')")]

let () =
  Arg.parse options (fun _ -> ()) "Compiles a While program to LLVM bitcode";
  let (decls, c) = parse_channel_fail parse_decls_cmd stdin in
  let array_max = StringMap.fold (fun _ v vs -> match v with
      | TypA(b, l) -> (b+l)::vs
      | _ -> vs) decls [] in
  let mem_size = List.fold_left max 0 array_max in
  let ibuilder = L.builder_at_end llcontext (L.entry_block fundef) in
  let memsize_v = L.const_int int_type mem_size in
  let mem =  L.build_array_alloca int_type memsize_v "mu" ibuilder in
  let zero_arr = Array.make mem_size zero in
  Array.iteri (fun i v ->
      let gep = L.build_gep mem [| (L.const_int int_type i) |] "idx" ibuilder in
      L.build_store v gep ibuilder |> ignore) zero_arr;
  let _ = exit_declaration in
  let printmu = printmu_declaration mem in
  let build_decls k v = (match v with
      | TypI       ->
        let local_var = L.build_alloca int_type k ibuilder in
        let _ = L.build_store zero local_var ibuilder in
        Int(local_var)
      | TypA(b, l) -> Arr(b,l)) in
  let sym_tab = StringMap.mapi build_decls decls in
  let ibuilder' = build_command c sym_tab mem ibuilder in
  let _ = L.build_call printmu [| mem; memsize_v |] "" ibuilder' in
  let _ = L.build_ret_void ibuilder' in
  print_string (Llvm.string_of_llmodule module_m);
  ignore(Llvm_bitwriter.write_bitcode_file module_m ! (output_file))
