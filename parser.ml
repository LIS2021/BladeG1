open Ast
open Opal

module StringMap = Map.Make(String)

(** List of reserved keywords in the WHILE language *)
let reserved_kws = [
  "if";
  "then";
  "else";
  "endif";
  "while";
  "do";
  "endwhile";
  "protect";
  "protect_fence";
  "protect_slh";
  "length";
  "base";
  "int"
]

(** Parses something inside parenthesis *)
let parens v = between (token "(") (token ")") v

(** Parses something inside brackets *)
let brackets v = between (token "[") (token "]") v

(** Parses an identifier *)
let ident = (spaces >> letter <~> many alpha_num) => implode >>= function
  | s when List.mem s reserved_kws -> mzero
  | s -> return s

(** Parses a number *)
let parse_number = spaces >> many1 digit => implode % int_of_string

(** Parses a number as an expression *)
let parse_number_expr = parse_number >>= fun n -> return (CstI n)

(** Parses a binary operator *)
let parse_op = token "+" <|> token "<" <|> token "&" <|> token "^"

(** Parses a primary value *)
let rec parse_value input =
  (parse_number_expr <|> parse_var) input

(** Parses an expression *)
and parse_expr input = (parse_length
                        <|> parse_base
                        <|> (parse_primary >>= fun e1 ->
                             token "?" >>
                             parse_expr >>= fun e2 ->
                             token ":" >>
                             parse_expr >>= fun e3 ->
                             return (InlineIf(e1,e2,e3)))
                        <|> (parse_primary >>= fun e1 ->
                             parse_op >>= fun op ->
                             parse_expr >>= fun e2 ->
                             return (BinOp(e1, e2, op)))
                        <|> parse_primary) input
and parse_primary input =
  (parse_value <|> (
      parens parse_expr >>= fun e ->
      return e
    )) input
and parse_var input =
  (ident >>= fun var -> return (Var var)) input
and parse_length input =
  (token "length" >>
   (parens ident) >>=
   fun var -> return (Length var)) input
and parse_base input =
  (token "base" >>
   (parens ident) >>=
   fun var -> return (Base var)) input

let parse_ptr_read input =
  (token "*" >>
   parse_expr >>= fun e ->
   return (PtrRead e)) input
let parse_arr_read input =
  (ident >>= fun id ->
   brackets parse_expr >>= fun e ->
   return (ArrayRead (id, e))) input
(** Parses the right-hand side of an assignment *)
let parse_rhs input =
  (parse_ptr_read
   <|> parse_arr_read
   <|> (parse_expr >>= fun e -> return (Expr e))) input

(** Parses a protect type *)
let parse_protect input =
  ( (token "protect_fence" >> return Fence)
    <|> (token "protect_slh" >> return Slh)
    <|> (token "protect" >> return Auto)) input

(** Parses a command *)
let rec parse_cmd input =
  ((parse_cmd_primary >>= fun c1 ->
    token ";" >>
    parse_cmd >>= fun c2 ->
    return (Seq (c1, c2)))
   <|> (parse_cmd_primary >>= fun c ->
        optional (token ";") >>
        return c)) input
and parse_cmd_primary input =
  let f_cmd : cmd = Fail in
  ((token "skip" >> return Skip)
   <|> (token "fail" >> return f_cmd)
   <|> parse_if
   <|> parse_while
   <|> parse_ptr_assign
   <|> parse_arr_assign
   <|> parse_protect_assign
   <|> parse_var_assign) input
and parse_if input =
  (token "if" >>
   parse_expr >>= fun e ->
   token "then" >>
   parse_cmd >>= fun c1 ->
   token "else" >>
   parse_cmd >>= fun c2 ->
   token "endif" >>
   return (If (e, c1, c2))) input
and parse_while input =
  (token "while" >>
   parse_expr >>= fun e ->
   token "do" >>
   parse_cmd >>= fun c ->
   token "endwhile" >>
   return (While (e, c))) input
and parse_ptr_assign input =
  (token "*" >>
   parse_expr >>= fun e1 ->
   token ":=" >>
   parse_expr >>= fun e2 ->
   return (PtrAssign (e1, e2))) input
and parse_arr_assign input =
  (ident >>= fun a ->
   brackets parse_expr >>= fun e1 ->
   token ":=" >>
   parse_expr >>= fun e2 ->
   return (ArrAssign (a, e1, e2))) input
and parse_var_assign input =
  (ident >>= fun v ->
   token ":=" >>
   parse_rhs >>= fun r ->
   return (VarAssign (v, r))) input
and parse_protect_assign input =
  (ident >>= fun v ->
   token ":=" >>
   parse_protect >>= fun p ->
   parens parse_rhs >>= fun r ->
   return (Protect (v, p, r))) input

(** Parses declarations followed by a command *)
let parse_decls_cmd input =
  let parse_type input =
    ((token "int" >> return TypI)
     <|> (brackets (parse_number >>= fun b ->
                    token "," >>
                    parse_number >>= fun l ->
                    return (TypA(b, l))))) input
  in let rec parse_decl input =
       (ident >>= fun ide ->
        token ":" >>
        parse_type >>= fun t ->
        token ";" >>
        return (ide, t)) input
  in let parse_decls input =
       (many parse_decl >>= fun decls ->
        return (List.fold_left (fun map (id, t) -> StringMap.add id t map) StringMap.empty decls)) input
  in (parse_decls >>= fun decls ->
      parse_cmd >>= fun c ->
      return (decls, c)) input

let parse_channel parser channel =
  LazyStream.of_channel channel |> parser

let parse_channel_fail parser s =
  match parse_channel parser s with
  | Some (x, _) -> x
  | _ -> failwith "Syntax error!"

let parse_string parser s =
  LazyStream.of_string s |> parser

let parse_string_fail parser s =
  match parse_string parser s with
  | Some (x, _) -> x
  | _ -> failwith "Syntax error!"