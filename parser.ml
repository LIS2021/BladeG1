open Ast
open Opal

let parse_stream stream =
  let reserved = [
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
    "base"
  ]
  in let parens v = between (token "(") (token ")") v
  in let brackets v = between (token "[") (token "]") v
  in let ident = (spaces >> letter <~> many alpha_num) => implode >>= function
    | s when List.mem s reserved -> mzero
    | s -> return s

  in let number = spaces >> many1 digit => implode % (fun x -> CstI (int_of_string x))

  in let parse_op = token "+" <|> token "<" <|> token "*"
  in let rec parse_value input =
       (number <|> parse_var) input
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

  in let parse_ptr_read input =
       (token "*" >>
        parse_expr >>= fun e ->
        return (PtrRead e)) input
  in let parse_arr_read input =
       (ident >>= fun id ->
        brackets parse_expr >>= fun e ->
        return (ArrayRead (id, e))) input
  in let parse_rhs input =
       (parse_ptr_read
        <|> parse_arr_read
        <|> (parse_expr >>= fun e -> return (Expr e))) input

  in let parse_protect input =
       ((token "protect" >>= fun _ -> return Auto)
        <|> (token "protect_slh" >>= fun _ -> return Slh)
        <|> (token "protect_fence" >>= fun _ -> return Fence)) input

  in let rec parse_cmd input =
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
  in match (parse_cmd stream) with
  | Some ((res, _)) -> Some res
  | None -> None

let parse_channel channel =
  LazyStream.of_channel channel |> parse_stream

let parse_channel_fail s =
  match parse_channel s with
  | Some x -> x
  | _ -> failwith "Syntax error!"

let parse_string s =
  LazyStream.of_string s |> parse_stream

let parse_string_fail s =
  match parse_string s with
  | Some x -> x
  | _ -> failwith "Syntax error!"