
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


      let compose (trans1 : _ transformation) (trans2 : _ transformation) : _ transformation =
        let open Result.Syntax in
        (fun stack ->
           let* (stack, ~depth_change:change1) = trans1 stack in
           let+ (stack, ~depth_change:change2) = trans2 stack in
           (stack, ~depth_change:(change1 + change2))
        )

      let unary operation : _ transformation =
        function
        | [] -> Error `Empty_stack
        | l::rest -> Ok ((operation l)::rest, ~depth_change:0) 

      let binary operation : _ transformation =
        function
        | [] -> Error `Empty_stack
        | [_] -> Error `Insufficient_arguments
        | l::r::rest -> Ok ((operation l r)::rest, ~depth_change:(-1)) 

      let swap : _ transformation =
        function
        | [] -> Error `Empty_stack
        | [_] -> Error `Insufficient_arguments
        | l::r::rest -> Ok (r::l::rest, ~depth_change:0) 

      let dup : _ transformation =
        function
        | [] -> Error `Empty_stack
        | l::rest -> Ok (l::l::rest, ~depth_change:1) 
      
      let add = binary ( + )

      let sub = compose swap (binary ( - ))

      let mul = binary ( * )

      let boolean_not = unary (fun i -> if i = 0 then 1 else 0)

      let bitwise_and = binary (land)

      let bitwise_or = binary (lor)

   end

   let transform (transformation : 'a Transformation.transformation) { stack ; depth; max_depth } =
     let open Result.Syntax in
     let* (stack, ~depth_change) = transformation stack in 
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
    then Error `Local_index_out_of_bound
    else Ok (CCRAL.set locals index value)  

end

type vm = {
  stack : Stack.t;
  locals : Locals.t;
  instructions : Vm_bytecode.instruction list
  
}

type vm_error =
  | Stack_overflow
  | Stack_underflow
  | Local_not_found of int 
  | Local_write_error of int
  | Insufficient_arguments of (name:string * arity:int * provided:int)

type vm_state =
  | Running of vm
  | Error of vm_error
  | Exited of int
  
let start_vm instructions = Running {
    stack = Stack.create ~max_depth:256;
    locals = Locals.create ~size:10;
    instructions
  }

let transformation_of_instruction =
  let open Vm_bytecode in
  let open Stack.Transformation in
  function
  | Swap -> Some swap
  | Add -> Some add
  | Sub -> Some sub
  | Mul -> Some mul
  | Dup -> Some dup
  | Or -> Some bitwise_or
  | And -> Some bitwise_and
  | Not -> Some boolean_not
  | _ -> None

let arity_error instruction ~provided = 
  let open Vm_bytecode in
  match instruction with
  | Swap -> (~name:"swap", ~arity:2, ~provided)
  | Add -> (~name:"add", ~arity:2, ~provided)
  | Sub -> (~name:"sub", ~arity:2, ~provided)
  | Mul -> (~name:"mul", ~arity:2, ~provided)
  | Dup -> (~name:"dup", ~arity:1, ~provided)
  | And -> (~name:"and", ~arity:2, ~provided)
  | Or -> (~name:"or", ~arity:2, ~provided)
  | Not -> (~name:"not", ~arity:1, ~provided)
  | _ -> (~name:"unknown", ~arity:0, ~provided)


let rec step vm_state =
  match vm_state with
  | Running vm -> step_from_vm vm
  | uncontinuable_state -> uncontinuable_state
and step_from_vm ({stack ; instructions ; locals } as vm) =
  let open Stack in
  match instructions with 
  | []
  | Exit::_ ->
      begin match pop stack with
      | Error `Underflow -> Exited (0)
      | Ok (~top, _) -> Exited top
      end
  | Nop::instructions -> Running ({vm with instructions})
  | (Push value)::instructions ->
      begin match push value stack with
      | Error `Overflow -> Error Stack_overflow
      | Ok stack -> Running { vm with stack; instructions }
      end
  | Pop::instructions ->
      begin match pop stack with
      | Error `Underflow -> Error Stack_underflow
      | Ok (stack, ..) -> Running {vm with stack; instructions }
      end
  | (Add as binary)::instructions
  | (Sub as binary)::instructions
  | (Mul as binary)::instructions
  | (Or as binary)::instructions
  | (And as binary)::instructions
  | (Swap as binary)::instructions ->
      begin match transform (Option.get @@ transformation_of_instruction binary) stack with
      | Error `Overflow -> Error Stack_overflow
      | Error `Empty_stack ->  Error (Insufficient_arguments (arity_error binary ~provided:0))
      | Error `Insufficient_arguments -> Error (Insufficient_arguments (arity_error binary ~provided:1))
      | Ok stack -> Running { vm with stack; instructions }
      end 
  | (Not as unary)::instructions
  | (Dup as unary)::instructions ->
      begin match transform (Option.get @@ transformation_of_instruction unary) stack with
      | Error `Overflow -> Error Stack_overflow
      | Error `Empty_stack ->  Error (Insufficient_arguments (arity_error unary ~provided:0))
      | Ok stack -> Running { vm with stack; instructions }
      | _ -> failwith "Can not happen"
      end 
  | (LoadLocal index)::instructions ->
      let local = Locals.load locals index in
      begin match local with
      | Error `Local_not_found -> Error (Local_not_found index)
      | Ok local ->
          begin match Stack.push local stack with
          | Error `Overflow -> Error Stack_overflow
          | Ok stack -> Running {vm with stack; instructions}
          end
      end      
  | (StoreLocal index)::instructions ->
      begin match pop stack with
      | Error `Underflow -> Error Stack_underflow
      | Ok (~top, stack) -> 
      begin match Locals.store locals index top with
      | Error `Local_index_out_of_bound -> Error (Local_write_error index)
      | Ok locals -> Running {stack; locals; instructions}
      end
      end


let run instructions =
  let vm = start_vm instructions in
  let rec loop vm =
    match vm with
    | Exited i -> Ok i
    | (Running _) as vm -> loop (step vm)
    | Error error -> Error error 
  in loop vm


