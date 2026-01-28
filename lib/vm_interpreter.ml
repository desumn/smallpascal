
module Stack = struct

  type t = {
    stack : int list; depth : int; max_depth : int
  }

  let create ~max_depth = {stack = []; depth = 0; max_depth} 

  let push value { stack; depth; max_depth } =
    if depth = max_depth then Error `Overflow else
    Ok { stack = value::stack; depth = depth + 1; max_depth}

  let pop =
    function
    | { stack = []; _ } -> Error `Stack_underflow
    | { stack = top::rs ; depth; _ } as stack ->
        Ok (top, { stack with stack = rs; depth = depth - 1})
end

module Locals = struct
  type t = int CCRAL.t

  let create ~size = CCRAL.make size 0
  
  let load locals int =
    match CCRAL.get locals int with
    | None -> Error `Local_not_found
    | Some i -> Ok i

  let store locals index value =
    if index >= CCRAL.length locals 
    then Ok (CCRAL.set locals index value)  
    else Error `Local_index_out_of_bound

end

type vm = {
  stack : Stack.t;
  locals : Locals.t;
  instructions : Vm_bytecode.instruction iarray
  
}

type vm_error =
  | Stack_overflow
  | Stack_underflow
  | Local_not_found of int 

type vm_state =
  | Running of vm
  | Error of vm_error
  | Exited of int
  
let start_vm instructions = Running {
    stack = Stack.create ~max_depth:256;
    locals = Locals.create ~size:10;
    instructions
  }

let rec step vm_state =
  match vm_state with
  | Running vm -> step_from_vm vm
  | uncontinuable_state -> uncontinuable_state
and step_from_vm vm =
  let open Result.Syntax in
  match vm.instructions with
  | [||] ->
      begin match Stack.pop vm.stack with
      | Error _ -> Exited 0
      | Ok (i, _) -> Exited i
      end
  | [|Nop; _|] -> Running vm
  | [|Push i; _|] ->
      begin match Stack.push i vm.stack with
      | Ok new_stack -> Running { vm with stack = new_stack }
      | Error `Overflow -> Error Stack_overflow 
      end 
  | _ -> Running vm
  

