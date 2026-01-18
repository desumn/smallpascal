
type token =
  | Begin | End
  | Program
  | Plus | Minus | Asterisk
  | Integer of int
  | Identifier of string
  | Eof


let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit]

let alpha = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']


let tokenize lexbuf =
  match%sedlex lexbuf with
  | "+" -> Plus
  | "-" -> Minus
  | "*" -> Asterisk
  | "begin" -> Begin
  | "end" -> End
  | "program" -> Program
  | number -> Integer (int_of_string @@ Sedlexing.Latin1.lexeme lexbuf)
  | alpha, Star (alpha | digit | '_') ->
      Identifier (Sedlexing.Latin1.lexeme lexbuf)
  | eof -> Eof
  | _ -> failwith "tbd: lexing error"





