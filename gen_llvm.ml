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

(** Function for looking up for llvalue of a name in the current table or in the global table 
    	@param name the name for which we are searching for
    	@param tab the map associated to current scope
*)
let lookup name tab =
  try
    StringMap.find name tab
  with
  | _ -> L.lookup_global name module_m |> Option.get

let tmp_count = ref 0

let gen_tmp_var =
  let name = Printf.sprintf "_%d" !tmp_count in
  incr tmp_count;
  name

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
    (L.build_zext le3 int_type "zext" ibuilder, ibuilder)
  |InlineIf(e, e1, e2)->
    (
      let cond_val, ibuilder = build_expr e tab mem ibuilder in
      let bool_val = L.build_icmp L.Icmp.Ne zero cond_val "cond" ibuilder in
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

let rec build_rhs rh tab mem ibuilder =
  let fail_func = L.lookup_function "fail" module_m |> Option.get in
  match rh with
  |Expr(expr) 		-> build_expr expr tab mem ibuilder
  |PtrRead(expr)		->
    let le, ibuilder = build_expr expr tab mem ibuilder in
    let gep = L.build_gep mem [|le|] "ptr" ibuilder in
    let load = L.build_load gep "load" ibuilder in
    (load, ibuilder)
  | ArrayRead(ide,expr)	->
    let cond_val, ibuilder = build_expr (BinOp(expr, Length(ide), "<")) tab mem ibuilder in
    let bool_val = L.build_icmp L.Icmp.Ne zero cond_val "cond" ibuilder in
    let bthen = L.append_block llcontext "then" fundef in
    let belse = L.append_block llcontext "else" fundef in
    let bcont = L.append_block llcontext "cont" fundef in
    let then_builder = L.builder_at_end llcontext bthen in
    let v1, bt = build_rhs (PtrRead(BinOp(Base(ide), expr, "+"))) tab mem then_builder in
    let _ = add_terminal bt (L.build_br bcont) in 
    let else_builder = L.builder_at_end llcontext belse in
    let _ = L.build_call fail_func [||] "" else_builder in
    let v2, be = build_expr (CstI 0) tab mem else_builder in
    let _ = add_terminal be (L.build_br bcont) in
    let _ = L.build_cond_br bool_val bthen belse ibuilder in
    let _ = L.position_at_end bcont ibuilder in
    let phi = L.build_phi [(v1, bthen); (v2, belse)] "phi" ibuilder in
    (phi, L.builder_at_end llcontext bcont)
and build_command c tab mem ibuilder =
  let fail_func = L.lookup_function "fail" module_m |> Option.get in
  match c with
  | Skip -> ibuilder
  | Fail -> let _ = L.build_call fail_func [||] "" ibuilder in ibuilder
  | VarAssign(id, rh) ->
    let rh_value, ibuilder = build_rhs rh tab mem ibuilder in
    let value = StringMap.find id tab in
    (match value with
     | Int(lvalue) -> let _ = L.build_store rh_value lvalue ibuilder in ibuilder 
     | Arr(_,_) -> failwith "Invalid assignment!")
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
  | Protect(id, p, ArrayRead(id_arr, e)) when p = Slh || p = Auto ->
    let e1 = BinOp(e, Length(id_arr), "<") in
    let e2 = BinOp(e, Base(id_arr), "+") in
    let tmp_var_name = gen_tmp_var in
    let tmp_var = L.build_alloca int_type tmp_var_name ibuilder in
    let tab = StringMap.add tmp_var_name (Int tmp_var) tab in
    let c1 = VarAssign(tmp_var_name, Expr e1) in
    let all_ones = Int.lognot 0 in
    let c2 = VarAssign(tmp_var_name, Expr (BinOp(CstI all_ones, Var tmp_var_name, "*"))) in
    let c3 = VarAssign(id, PtrRead(BinOp(e2, Var tmp_var_name, "&"))) in
    let c' = Seq(c1, If(Var tmp_var_name, Seq(c2, c3), Fail)) in
    build_command c' tab mem ibuilder
  | Protect(id, _, r) ->
    let v, ibuilder = build_rhs r tab mem ibuilder in
    L.build_fence L.AtomicOrdering.SequentiallyConsistent false ibuilder |> ignore;
    let value = StringMap.find id tab in
    (match value with
     | Int(lvalue) -> let _ = L.build_store v lvalue ibuilder in ibuilder 
     | Arr(_,_) -> failwith "Invalid assignment!") |> ignore;
    ibuilder

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
