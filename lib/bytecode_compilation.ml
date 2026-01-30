open Parsing
open Vm_bytecode

let instruction_of_operator operator : [unary_operator | binary_operator] =
  match operator with
  | `Plus -> `Add
  | `Minus -> `Sub
  | `Times -> `Mul
  | `Or -> `Or
  | `And -> `And
  | `Not -> `Not


let rec compile program : instruction list =
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
    @ [`Exit]
and compile_expression =
  function
  | Boolean true -> [`Push 1]
  | Boolean false -> [`Push 0]
  | Integer i -> [`Push i]
  | UnaryOperation {operator ; expression } ->
    compile_expression expression
    @ [(instruction_of_operator operator :> instruction)]
  | BinaryOperation { operator; left; right } ->
    compile_expression left
    @ compile_expression right
    @ [(instruction_of_operator operator :> instruction)]
