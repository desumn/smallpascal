
module Stack = struct

  type stack = int list

  type t = {
    stack : stack; depth : int; max_depth : int
  }

  let create ~max_depth = {stack = []; depth = 0; max_depth} 

  let push value { stack; depth; max_depth } =
    if depth = max_depth then Error `Overflow else
    Ok { stack = value::stack; depth = depth + 1; max_depth}

  let pop =
    function
    | { stack = []; _ } -> Error `Underflow
    | { stack = top::rs ; depth; _ } as stack ->
        Ok (~top, { stack with stack = rs; depth = depth - 1})

   module Transformation = struct

      type 'error transformation = stack -> (stack * depth_change:int, 'error) result

      let unary operation : _ transformation =
        function
        | [] -> Error `Empty_stack
        | l::rest -> Ok ((operation l)::rest, ~depth_change:0) 

      let binary operation : _ transformation =
        function
        | [] -> Error `Empty_stack
        | [_] -> Error `Insufficient_arguments
        | l::r::rest -> Ok ((operation l r)::rest, ~depth_change:(-1)) 

      let add = binary ( + )

      let sub = binary ( - )

      let mul = binary ( * )

      let swap : _ transformation =
        function
        | [] -> Error `Empty_stack
        | [_] -> Error `Insufficient_arguments
        | l::r::rest -> Ok (r::l::rest, ~depth_change:0) 

      let dup : _ transformation =
        function
        | [] -> Error `Empty_stack
        | l::rest -> Ok (l::l::rest, ~depth_change:1) 
      
   end

   let transform transformation { stack ; depth; max_depth } =
     let (stack, ~depth_change) = transformation stack in 
     if depth + depth_change > max_depth
     then Error `Overflow
     else Ok { stack; depth = depth + depth_change; max_depth}

  
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
and step_from_vm vm = Exited 0
