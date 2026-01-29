open Parsing
open Vm_bytecode

let instruction_of_operator =
  function
  | `Plus -> Add
  | `Minus -> Sub
  | `Times -> Mul


let rec compile program =
  match program with
  | Program {main_block ; _ } -> compile_block main_block
and compile_block block =
  block
  |> List.map (compile_statement)
  |> List.flatten
and compile_statement : statement -> instruction list =
  function
  | Exit expr ->
    compile_expression expr
    @ [Exit]
and compile_expression =
  function
  | Integer i -> [Push i]
  | BinaryOperation { operator; left; right } ->
    compile_expression left
    @ compile_expression right
    @ [instruction_of_operator operator]
