
type instruction =
  | Nop


  | Push of int
  | Pop
  | Swap
  | Dup

  | LoadLocal of int
  | StoreLocal of int
  

  | Add | Sub | Mul
  | Or | And | Not

  | Exit
  
