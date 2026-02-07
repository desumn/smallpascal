

type zero_to_zero = [`Nop | `Exit]
and one_to_one = [`Not]
and two_to_two = [`Swap]
and one_to_two = [`Dup] 
and two_to_one =
  [ `Add | `Sub | `Mul | `Or | `And |
    `Equal | `NotEqual | `Greater | `Lesser |
    `GreaterEqual | `LesserEqual ]
and basic_manipulation = [`Push of int | `Pop]
and locals = [`StoreLocal of int | `LoadLocal of int]
and control = [`Jump of int | `JumpIfZero of int]


type instruction = [ zero_to_zero | one_to_one | two_to_two |
                     one_to_two | two_to_one | basic_manipulation | locals | control]

type unary_operator = [one_to_one | one_to_two]
type binary_operator = [two_to_one | two_to_two]
type operator = [unary_operator | binary_operator]
