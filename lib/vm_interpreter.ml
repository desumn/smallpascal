open Vm_bytecode



module Stack = struct

  type t = { mutable sp : int; data : int array}

  let create ~depth = {sp = 0; data = Array.make depth 0}

  let push stack value =
    stack.data.(stack.sp) <- value; 
    stack.sp <- stack.sp + 1

  let pop stack =
    stack.sp <- stack.sp - 1;
    stack.data.(stack.sp)

  let unary operation stack =
    let operand = pop stack in
    List.iter (push stack) (operation operand)

  let binary operation stack =
    let right = pop stack in 
    let left = pop stack in
    List.iter (push stack) (operation left right)

  let comparison operation = binary (fun left right -> if operation left right then [1] else [0])

  let flat1 operation operand = [operation operand]
  let flat2 operation left right = [operation left right]
  
  let swap = binary (fun left right -> [right; left]) 
  let dup = unary (fun operand -> [operand ; operand])

  let add = binary (flat2 (+))
  let sub stack = swap stack; binary (flat2 (-)) stack
  let mul = binary (flat2 ( * ))

  let bitwise_not = unary (flat1 (lnot))
  let bitwise_or = binary (flat2 (lor))
  let bitwise_and = binary (flat2 (land))

  let equal = comparison (=)
  let not_equal = comparison (<>)
  let greater = comparison (>)
  let lesser = comparison (<)
  let greater_equal = comparison (>=)
  let lesser_equal = comparison (<=)

end

module Locals = struct
  type t = int array

  let create ~size = Array.make size 0
  
  let load locals index = locals.(index)

  let store locals index value = locals.(index) <- value

end

module Instructions = struct

  type t = {
    mutable pc : int;
    data : Vm_bytecode.instruction iarray
  }

  let create instructions = { pc = 0; data = instructions }

  let next_instruction instructions =
    let value = Iarray.get instructions.data (instructions.pc) in
    instructions.pc <- instructions.pc + 1; value
  
  let jump instructions pos = instructions.pc <- pos 

end


type vm = {
  stack : Stack.t;
  locals : Locals.t;
  instructions : Instructions.t
  
}

type vm_state =
  | Running of vm
  | Exited of int
  
let start_vm instructions = Running {
    stack = Stack.create ~depth:256;
    locals = Locals.create ~size:10;
    instructions = Instructions.create instructions
  }

let transformation_of_instruction (instruction : [unary_operator | binary_operator]) =
  let open Stack in
  match instruction with
  | `Swap -> swap
  | `Add -> add
  | `Sub -> sub
  | `Mul -> mul
  | `Dup -> dup
  | `Or -> bitwise_or
  | `And -> bitwise_and
  | `Not -> bitwise_not
  | `Equal -> equal
  | `NotEqual -> not_equal
  | `Greater -> greater
  | `Lesser -> lesser
  | `GreaterEqual -> greater_equal
  | `LesserEqual -> lesser_equal

let rec step vm_state =
  match vm_state with
  | Running vm -> step_from_vm vm
  | uncontinuable_state -> uncontinuable_state
and step_from_vm ({stack ; instructions ; locals } as vm) =
  let open Stack in
  match Instructions.next_instruction instructions with 
  | `Exit -> Exited (pop stack)
  | `Nop -> Running vm
  | (`Push value) -> push stack value; Running vm
  | `Pop -> ignore @@ pop stack; Running vm
  | (#unary_operator | #binary_operator as operator) -> transformation_of_instruction operator stack; Running vm
  | (`LoadLocal index) -> push stack (Locals.load locals index); Running vm
  | (`StoreLocal index) ->
      let value = pop stack in
      Locals.store locals index value; Running vm
  | `Jump address -> Instructions.jump instructions address; Running vm
  | `JumpIfZero address ->
      let conditional = pop stack in 
      if conditional = 0
      then (Instructions.jump instructions address; Running vm)
      else Running vm

