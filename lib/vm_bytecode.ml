
type instruction =
  | Nop


  | Push
  | Pop
  | Swap

  | LoadLocal of int
  | StoreLocal of int
  

  | Add | Sub | Mul
