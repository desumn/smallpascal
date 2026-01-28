

module Stack = struct

  type t = {
    stack : int list; depth : int; max_depth : int
  }

  let push value { stack; depth; max_depth } =
    if depth = max_depth then Error `Overflow else
    Ok { stack = value::stack; depth = depth + 1; max_depth}

  let pop =
    function
    | { stack = []; _ } -> Error `Stack_underflow
    | { stack = top::rs ; depth; _ } as stack ->
        Ok (~top, { stack with stack = rs; depth = depth - 1})
    

end

module Locals = struct
  type t = int CCRAL.t

  let load locals int =
    match CCRAL.get locals int with
    | None -> Error `Local_not_found
    | Some i -> Ok i

  let store = CCRAL.set  

end



type vm = {
  stack : Stack.t;
  locals : Locals.t
  
}

type vm_error =
  | Stack_overflow
  | Stack_underflow
  | Local_not_found of int 


type vm_state =
  | Running of vm
  | Error of vm_error
  | Exited of int
  
