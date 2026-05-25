(** 字节码虚拟机 *)

open Ast
open Bytecode

exception VMError of string

(** 运行时值 *)
type vm_value =
  | VInt of int
  | VBool of bool
  | VString of string
  | VUnit
  | VNil
  | VList of vm_value list
  | VClosure of (string * vm_value) list * string * instr array

let rec string_of_vm_value = function
  | VInt n -> string_of_int n
  | VBool true -> "true"
  | VBool false -> "false"
  | VString s -> "\"" ^ s ^ "\""
  | VUnit -> "()"
  | VNil -> "[]"
  | VList vs -> "[" ^ String.concat "; " (List.map string_of_vm_value vs) ^ "]"
  | VClosure _ -> "<closure>"

(** VM 帧 *)
type frame = {
  mutable pc : int;
  code : instr array;
  mutable stack : vm_value list;
  mutable env : (string * vm_value) list;
}

type vm = {
  mutable frames : frame list;
}

let lookup env x =
  match List.assoc_opt x env with
  | Some v -> v
  | None -> raise (VMError ("Unbound variable: " ^ x))

let push frame v = frame.stack <- v :: frame.stack

let pop frame =
  match frame.stack with
  | v :: rest ->
      frame.stack <- rest;
      v
  | [] -> raise (VMError "Stack underflow")

let run code =
  let initial_frame = { pc = 0; code; stack = []; env = [] } in
  let vm = { frames = [initial_frame] } in
  
  while vm.frames <> [] do
    let frame = List.hd vm.frames in
    
    if frame.pc >= Array.length frame.code then begin
      (* 帧结束，弹出并传递结果 *)
      let result =
        match frame.stack with
        | [v] -> v
        | [] -> VUnit
        | v :: _ -> v
      in
      vm.frames <- List.tl vm.frames;
      if vm.frames <> [] then
        push (List.hd vm.frames) result
      else if frame == initial_frame then
        (* 主帧结束，将结果留在栈上以便返回 *)
        initial_frame.stack <- [result]
    end else begin
      let instr = frame.code.(frame.pc) in
      frame.pc <- frame.pc + 1;
      
      match instr with
      | PushInt n -> push frame (VInt n)
      | PushBool b -> push frame (VBool b)
      | PushString s -> push frame (VString s)
      | PushUnit -> push frame VUnit
      | PushNil -> push frame VNil
      | LoadVar x -> push frame (lookup frame.env x)
      | StoreVar x ->
          let v = pop frame in
          frame.env <- (x, v) :: frame.env
      | Add ->
          (match pop frame, pop frame with
           | VInt b, VInt a -> push frame (VInt (a + b))
           | _ -> raise (VMError "Type error: + requires integers"))
      | Sub ->
          (match pop frame, pop frame with
           | VInt b, VInt a -> push frame (VInt (a - b))
           | _ -> raise (VMError "Type error: - requires integers"))
      | Mul ->
          (match pop frame, pop frame with
           | VInt b, VInt a -> push frame (VInt (a * b))
           | _ -> raise (VMError "Type error: * requires integers"))
      | Div ->
          (match pop frame, pop frame with
           | VInt 0, VInt _ -> raise (VMError "Division by zero")
           | VInt b, VInt a -> push frame (VInt (a / b))
           | _ -> raise (VMError "Type error: / requires integers"))
      | Eq ->
          (match pop frame, pop frame with
           | VInt b, VInt a -> push frame (VBool (a = b))
           | VBool b, VBool a -> push frame (VBool (a = b))
           | VString b, VString a -> push frame (VBool (a = b))
           | _ -> push frame (VBool false))
      | Neq ->
          (match pop frame, pop frame with
           | VInt b, VInt a -> push frame (VBool (a <> b))
           | VBool b, VBool a -> push frame (VBool (a <> b))
           | VString b, VString a -> push frame (VBool (a <> b))
           | _ -> push frame (VBool true))
      | Lt ->
          (match pop frame, pop frame with
           | VInt b, VInt a -> push frame (VBool (a < b))
           | _ -> raise (VMError "Type error: < requires integers"))
      | Le ->
          (match pop frame, pop frame with
           | VInt b, VInt a -> push frame (VBool (a <= b))
           | _ -> raise (VMError "Type error: <= requires integers"))
      | Gt ->
          (match pop frame, pop frame with
           | VInt b, VInt a -> push frame (VBool (a > b))
           | _ -> raise (VMError "Type error: > requires integers"))
      | Ge ->
          (match pop frame, pop frame with
           | VInt b, VInt a -> push frame (VBool (a >= b))
           | _ -> raise (VMError "Type error: >= requires integers"))
      | And ->
          (match pop frame, pop frame with
           | VBool b, VBool a -> push frame (VBool (a && b))
           | _ -> raise (VMError "Type error: && requires booleans"))
      | Or ->
          (match pop frame, pop frame with
           | VBool b, VBool a -> push frame (VBool (a || b))
           | _ -> raise (VMError "Type error: || requires booleans"))
      | Not ->
          (match pop frame with
           | VBool b -> push frame (VBool (not b))
           | _ -> raise (VMError "Type error: not requires boolean"))
      | Jump addr -> frame.pc <- addr
      | JumpIfFalse addr ->
          (match pop frame with
           | VBool false -> frame.pc <- addr
           | VBool true -> ()
           | _ -> raise (VMError "Type error: if requires boolean"))
      | MakeClosure (param, func_code) ->
          (* 简化：不支持真正的闭包，只存储空环境 *)
          push frame (VClosure ([], param, func_code))
      | Call ->
          (match pop frame with
           | VClosure (_, param, func_code) ->
               let arg = pop frame in
               let new_frame = {
                 pc = 0;
                 code = func_code;
                 stack = [];
                 env = (param, arg) :: frame.env
               } in
               vm.frames <- new_frame :: vm.frames
           | _ -> raise (VMError "Type error: call requires function"))
      | Return ->
          frame.pc <- Array.length frame.code
      | MakeList n ->
          let rec loop acc n =
            if n = 0 then acc
            else loop (pop frame :: acc) (n - 1)
          in
          push frame (VList (loop [] n))
      | Cons ->
          (match pop frame, pop frame with
           | VList tail, head -> push frame (VList (head :: tail))
           | _ -> raise (VMError "Type error: :: requires a list"))
      | Head ->
          (match pop frame with
           | VList (h :: _) -> push frame h
           | VList [] -> raise (VMError "head: empty list")
           | _ -> raise (VMError "head: expected list"))
      | Tail ->
          (match pop frame with
           | VList (_ :: t) -> push frame (VList t)
           | VList [] -> raise (VMError "tail: empty list")
           | _ -> raise (VMError "tail: expected list"))
      | Length ->
          (match pop frame with
           | VList l -> push frame (VInt (List.length l))
           | VString s -> push frame (VInt (String.length s))
           | _ -> raise (VMError "length: expected list or string"))
      | Concat ->
          (match pop frame, pop frame with
           | VString b, VString a -> push frame (VString (a ^ b))
           | _ -> raise (VMError "Type error: ^ requires strings"))
      | Print ->
          let v = pop frame in
          print_endline (string_of_vm_value v);
          push frame VUnit
      | Pop -> ignore (pop frame)
      | Dup ->
          match frame.stack with
          | v :: _ -> push frame v
          | [] -> raise (VMError "Stack underflow")
    end
  done;
  
  match initial_frame.stack with
  | [v] -> v
  | [] -> VUnit
  | v :: _ -> v
