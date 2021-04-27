open Ast

module StringMap = Map.Make(String)

type value = 
  | Ival of int
  | Aval of int * int
  | Pval of int

(*         DIRECTIVES         *)
type prediction = bool

type directive =
  | Fetch
  | PFetch of prediction
  | Exec of int
  | Retire

(*         OBSERVATIONS         *)
type observation =
  | None
  | Read of int * int list
  | Write of int * int list
  | Fail of int
  | Rollback of int

(*        INSTRUCTION SET        *)
type instruction =
  | Nop
  | AssignV of identifier * value
  | AssignE of identifier * expr
  | Load of identifier * expr                   (*     id := load(e)         *)
  | StoreV of value * value
  | StoreE of expr * expr
  | IProtectV of identifier * protect * value
  | IProtectE of identifier * protect * expr     (*     id := protect(e)     *)
  | Guard of expr * prediction * cmd list * int
  | Fail of int 

(*       CONFIGURATIONS        *)
type configuration = {
  is : instruction list ; 
  cs : cmd list ; 
  mu : int array ; 
  rho : int StringMap.t ;
} [@@deriving show]