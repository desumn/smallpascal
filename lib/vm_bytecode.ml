

type zero_to_zero = [`Nop | `Exit]
and zero_to_one = [`Push of int]
and one_to_zero = [`Pop]
and one_to_one = [`Not]
and two_to_two = [`Swap]
and one_to_two = [`Dup] 
and two_to_one =
  [ `Add | `Sub | `Mul | `Or | `And |
    `Equal | `NotEqual | `Greater | `Lesser |
    `GreaterEqual | `LesserEqual ]
and locals = [`StoreLocal of int | `LoadLocal of int]
and control = [`Jump of int | `JumpIfZero of int]


type instruction = [ zero_to_zero | zero_to_one | one_to_zero | one_to_one | two_to_two |
                     one_to_two | two_to_one | locals | control]

type unary_operator = [one_to_one | one_to_two]
type binary_operator = [two_to_one | two_to_two]
