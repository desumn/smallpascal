open Vm_bytecode

let depth_change : operator -> int =
  function
  | #one_to_one
  | #two_to_two -> 0
  | #two_to_one -> -1
  | #one_to_two -> 1

type verifier_state =
    | Valid of (instruction iarray * max_depth:int * locals_size:int)
    | Error of [`Stack_underflow | `Negative_index | `Negative_address | `Address_too_high | `Index_too_high]

let read_instruction instructions =
    Iarray.get instructions 1, Iarray.sub ~pos:1 ~len:(Iarray.length instructions - 1) instructions


let verify initial_instructions =
  let address_space = Iarray.length initial_instructions in
  let rec loop (instructions : instruction iarray) depth max_depth locals_size =
    if instructions = [||]
    then Valid (initial_instructions, ~max_depth, ~locals_size)
    else match read_instruction instructions with
    | `Exit, _ -> Valid (initial_instructions, ~max_depth, ~locals_size)
    | `Nop, rest -> loop rest depth max_depth locals_size
    | (#operator as op), rest ->
        let new_depth = depth + (depth_change op) in
        if new_depth < 0 then Error `Stack_underflow
        else loop rest new_depth (max new_depth max_depth) locals_size 
    | (`Push _), rest ->
        loop rest (depth + 1) (max (depth + 1) max_depth) locals_size          
    | `Pop, rest ->
       if depth - 1 < 0
       then Error `Stack_underflow
       else loop rest (depth - 1) max_depth locals_size
    | (`Jump address), rest
    | (`JumpIfZero address), rest -> 
        if address < 0 then Error `Negative_index
        else if address > address_space then (Error `Address_too_high)
        else loop (Iarray.sub ~pos:address ~len:(address_space - address) rest)
                  depth max_depth locals_size
    | `StoreLocal index, rest ->
       if index < 0
       then Error `Negative_index
       else loop rest (depth - 1) max_depth (max locals_size index)
    | `LoadLocal index, rest ->
       if index < 0
       then Error `Negative_index
       else if index > locals_size
       then Error `Index_too_high
       else loop rest (depth + 1) (max (depth + 1) max_depth) locals_size
    in
  loop initial_instructions 0 0 0
     






