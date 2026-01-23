open Result.Syntax

type token =
  | Begin
  | End
  | Program
  | Plus
  | Minus
  | Asterisk
  | Integer of int
  | Identifier of string
  | OpenParen
  | CloseParen
  | Semicolon
  | Eof

let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit]
let alpha = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']

let rec tokenize lexbuf =
  match%sedlex lexbuf with
  | white_space | '\n' -> tokenize lexbuf
  | "+" -> Plus
  | "-" -> Minus
  | "*" -> Asterisk
  | "begin" -> Begin
  | "end" -> End
  | "(" -> OpenParen
  | ")" -> CloseParen
  | ";" -> Semicolon
  | "program" -> Program
  | number -> Integer (int_of_string @@ Sedlexing.Latin1.lexeme lexbuf)
  | alpha, Star (alpha | digit | '_') ->
      Identifier (Sedlexing.Latin1.lexeme lexbuf)
  | eof -> Eof
  | _ -> failwith "tbd: lexing error"

type program = Program of { name : string; main_block : block }
and block = statement list
and statement = Exit of expression

and expression =
  | Integer of int
  | BinaryOperation of {
      operator : [ `Plus | `Minus | `Times ];
      left : expression;
      right : expression;
    }

let operator_of_token = function
  | Plus -> `Plus
  | Minus -> `Minus
  | Asterisk -> `Times
  | _ -> assert false

let parse_between lexbuf ~opening ~closing ~parser =
  match tokenize lexbuf with
  | token when token = opening ->
      let sub_expr = parser lexbuf in
      begin match tokenize lexbuf with
      | token when token = closing -> sub_expr
      | _ -> Error "Invalid closing"
      end
  | _ -> Error "Invalid opening"

let parse_separated_sequence lexbuf ~separators ~transform ~parser ~join
    ~encapsulate =
  let* first_element = parser lexbuf in
  let rec loop acc =
    match tokenize lexbuf with
    | token when List.mem token separators ->
        let operator = transform token in
        let* next = parser lexbuf in
        loop (join next operator acc)
    | _ ->
        Sedlexing.rollback lexbuf;
        Ok acc
  in
  loop (encapsulate first_element)

let rec parse lexbuf =
  match tokenize lexbuf with
  | Program -> begin
      match tokenize lexbuf with
      | Identifier name ->
          let* main_block = parse_block lexbuf in
          Ok (Program { name; main_block })
      | _ -> Error "Missing or invalid program name"
    end
  | _ -> Error "Missing program declaration"

and parse_block lexbuf =
  parse_between lexbuf ~opening:Begin ~closing:End
    ~parser:
      (parse_separated_sequence ~separators:[ Semicolon ]
         ~transform:(fun _ -> ())
         ~parser:parse_statement
         ~encapsulate:(fun el -> [ el ])
         ~join:(fun hd _ tl -> hd :: tl))

and parse_statement lexbuf =
  match tokenize lexbuf with
  | Identifier "exit" ->
      let* expr =
        parse_between lexbuf ~opening:OpenParen ~closing:CloseParen
          ~parser:parse_expression
      in
      Ok (Exit expr)
  | _ -> Error "Invalid statement"

and parse_expression lexbuf =
  let terms =
    parse_separated_sequence lexbuf ~separators:[ Plus; Minus ]
      ~parser:parse_term ~transform:operator_of_token
      ~join:(fun right operator left ->
        BinaryOperation { operator; left; right })
      ~encapsulate:Fun.id
  in
  terms

and parse_term lexbuf =
  let factors =
    parse_separated_sequence lexbuf ~separators:[ Asterisk ]
      ~parser:parse_factor ~transform:operator_of_token
      ~join:(fun right operator left ->
        BinaryOperation { operator; left; right })
      ~encapsulate:Fun.id
  in
  factors

and parse_factor lexbuf =
  match tokenize lexbuf with
  | Integer i -> Ok (Integer i)
  | OpenParen ->
      Sedlexing.rollback lexbuf;
      parse_between lexbuf ~opening:OpenParen ~closing:CloseParen
        ~parser:parse_expression
  | _ -> Error "Invalid factor"
