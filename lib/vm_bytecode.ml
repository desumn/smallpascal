
type instruction =
  | Nop


  | Push of int
  | Pop
  | Swap

  | LoadLocal of int
  | StoreLocal of int
  

  | Add | Sub | Mul
