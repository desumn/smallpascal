open Parsing
open Printf

type label = string

let label s = s
let emit_label = sprintf "%s:"

type directive = Section of [ `Text ] | Global of label

let emit_directive = function
  | Section `Text -> ".section .text"
  | Global label -> sprintf ".globl %s" label

type register =
  | A0
  | A1
  | A2
  | A3
  | A4
  | A5
  | A6
  | A7
  | T0
  | T1
  | T2
  | T3
  | T4
  | T5
  | T6

let register_mappings =
  [
    (A0, "a0");
    (A1, "a1");
    (A2, "a2");
    (A3, "a3");
    (A4, "a4");
    (A5, "a5");
    (A6, "a6");
    (A7, "a7");
    (T0, "t0");
    (T1, "t1");
    (T2, "t2");
    (T3, "t3");
    (T4, "t4");
    (T5, "t5");
    (T6, "t6");
  ]

let emit_register reg = List.assoc reg register_mappings

type immediate = Integer of int

let emit_immediate = function Integer int -> string_of_int int

type operand = Immediate of immediate | Register of register

let emit_operand = function
  | Immediate imm -> emit_immediate imm
  | Register reg -> emit_register reg

type instruction =
  | LoadImmediate of { destination_register : operand; immediate : operand }
  | UnaryRegisterInstruction of {
      operation : [ `Mv ];
      destination_register : operand;
      source_register : operand;
    }
  | BinaryRegisterInstruction of {
      operation : [ `Add | `Sub | `Mul ];
      destination_register : operand;
      source_register_1 : operand;
      source_register_2 : operand;
    }
  | ECall

let string_of_operation = function
  | `Add -> "add"
  | `Sub -> "sub"
  | `Mul -> "mul"
  | `Mv -> "mv"

let emit_instruction = function
  | LoadImmediate { destination_register; immediate } ->
      sprintf "%4s %s, %s" "li"
        (emit_operand destination_register)
        (emit_operand immediate)
  | UnaryRegisterInstruction
      { operation; destination_register; source_register } ->
      sprintf "%4s %s, %s"
        (string_of_operation operation)
        (emit_operand destination_register)
        (emit_operand source_register)
  | BinaryRegisterInstruction
      { operation; destination_register; source_register_1; source_register_2 }
    ->
      sprintf "%4s %s, %s, %s"
        (string_of_operation operation)
        (emit_operand destination_register)
        (emit_operand source_register_1)
        (emit_operand source_register_2)
  | ECall -> sprintf "%4s" "ecall"

let compile_operator = function
  | `Plus -> `Add
  | `Minus -> `Sub
  | `Times -> `Mul

type top_level =
  | Directive of directive
  | Label of label
  | Instruction of instruction

let tl_directive directive = Directive directive
let tl_label label = Label label
let tl_instruction instruction = Instruction instruction

let emit = function
  | Directive directive -> emit_directive directive
  | Label label -> emit_label label
  | Instruction instruction -> emit_instruction instruction

let section section_type = tl_directive @@ Section section_type
let global label = tl_directive @@ Global label

let li dest immediate =
  tl_instruction
  @@ LoadImmediate
       { destination_register = Register dest; immediate = Immediate immediate }

let ecall = tl_instruction ECall

let unary_register_instruction operation dest src =
  tl_instruction
  @@ UnaryRegisterInstruction
       {
         operation;
         destination_register = Register dest;
         source_register = Register src;
       }

let mv = unary_register_instruction `Mv

let binary_register_instruction operation dest src1 src2 =
  tl_instruction
  @@ BinaryRegisterInstruction
       {
         operation;
         destination_register = Register dest;
         source_register_1 = Register src1;
         source_register_2 = Register src2;
       }

let add = binary_register_instruction `Add
let sub = binary_register_instruction `Sub
let mul = binary_register_instruction `Mul

let rec compile program =
  match program with
  | Program { main_block; _ } ->
      let start = label "_start" in
      [ section `Text; global start; tl_label start ] @ compile_block main_block

and compile_block block = block |> List.map compile_statement |> List.flatten

and compile_statement statement =
  match statement with
  | Exit expr -> compile_expression expr @ [ li A7 (Integer 93); ecall ]

and compile_expression expression =
  match expression with
  | Integer int -> [ li A0 (Integer int) ]
  | BinaryOperation { operator; left; right } ->
      compile_expression left
      @ [ mv T1 A0 ]
      @ compile_expression right
      @ [ binary_register_instruction (compile_operator operator) A0 T1 A0 ]

let emit_program program =
  compile program |> List.map emit |> CCList.to_string Fun.id ~sep:"\n"
