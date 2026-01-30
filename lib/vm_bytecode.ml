

type no_change = [ `Nop | `Exit ]
and adder = [ `Push of int | `LoadLocal of int ]
and remover = [ `Pop | `StoreLocal of int ]
and unary_operator = [ `Dup | `Not ]
and binary_operator = [ `Add | `Sub | `Mul | `Or | `And | `Swap ]

type instruction = [ no_change | adder | remover | unary_operator | binary_operator ]
