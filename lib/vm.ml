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
  | VClosure of (string * vm_value) list * string * instr array * string option

let rec string_of_vm_value = function
  | VInt n -> string_of_int n
  | VBool true -> "true"
  | VBool false -> "false"
  | VString s -> "\"" ^ s ^ "\""
  | VUnit -> "()"
  | VNil -> "[]"
  | VList vs -> "[" ^ String.concat "; " (List.map string_of_vm_value vs) ^ "]"
  | VClosure _ -> "<closure>"

let lookup env x =
  match List.assoc_opt x env with
  | Some v -> v
  | None -> raise (VMError ("Unbound variable: " ^ x))

(** 执行字节码 *)
let run code =
  let stack = ref [] in
  let env = ref [] in
  let pc = ref 0 in
  let call_stack = ref [] in
  
  let push v = stack := v :: !stack in
  let pop () =
    match !stack with
    | v :: rest -> stack := rest; v
    | [] -> raise (VMError "Stack underflow")
  in
  
  let rec execute () =
    while !pc < Array.length code do
      let instr = code.(!pc) in
      pc := !pc + 1;
      
      match instr with
      | PushInt n -> push (VInt n)
      | PushBool b -> push (VBool b)
      | PushString s -> push (VString s)
      | PushUnit -> push VUnit
      | PushNil -> push VNil
      | LoadVar x -> push (lookup !env x)
      | StoreVar x -> env := (x, pop ()) :: !env
      | Add ->
          (match pop (), pop () with
           | VInt b, VInt a -> push (VInt (a + b))
           | _ -> raise (VMError "Type error: + requires integers"))
      | Sub ->
          (match pop (), pop () with
           | VInt b, VInt a -> push (VInt (a - b))
           | _ -> raise (VMError "Type error: - requires integers"))
      | Mul ->
          (match pop (), pop () with
           | VInt b, VInt a -> push (VInt (a * b))
           | _ -> raise (VMError "Type error: * requires integers"))
      | Div ->
          (match pop (), pop () with
           | VInt 0, VInt _ -> raise (VMError "Division by zero")
           | VInt b, VInt a -> push (VInt (a / b))
           | _ -> raise (VMError "Type error: / requires integers"))
      | Eq ->
          (match pop (), pop () with
           | VInt b, VInt a -> push (VBool (a = b))
           | VBool b, VBool a -> push (VBool (a = b))
           | VString b, VString a -> push (VBool (a = b))
           | _ -> push (VBool false))
      | Neq ->
          (match pop (), pop () with
           | VInt b, VInt a -> push (VBool (a <> b))
           | VBool b, VBool a -> push (VBool (a <> b))
           | VString b, VString a -> push (VBool (a <> b))
           | _ -> push (VBool true))
      | Lt ->
          (match pop (), pop () with
           | VInt b, VInt a -> push (VBool (a < b))
           | _ -> raise (VMError "Type error: < requires integers"))
      | Le ->
          (match pop (), pop () with
           | VInt b, VInt a -> push (VBool (a <= b))
           | _ -> raise (VMError "Type error: <= requires integers"))
      | Gt ->
          (match pop (), pop () with
           | VInt b, VInt a -> push (VBool (a > b))
           | _ -> raise (VMError "Type error: > requires integers"))
      | Ge ->
          (match pop (), pop () with
           | VInt b, VInt a -> push (VBool (a >= b))
           | _ -> raise (VMError "Type error: >= requires integers"))
      | And ->
          (match pop (), pop () with
           | VBool b, VBool a -> push (VBool (a && b))
           | _ -> raise (VMError "Type error: && requires booleans"))
      | Or ->
          (match pop (), pop () with
           | VBool b, VBool a -> push (VBool (a || b))
           | _ -> raise (VMError "Type error: || requires booleans"))
      | Not ->
          (match pop () with
           | VBool b -> push (VBool (not b))
           | _ -> raise (VMError "Type error: not requires boolean"))
      | Jump addr -> pc := addr
      | JumpIfFalse addr ->
          (match pop () with
           | VBool false -> pc := addr
           | VBool true -> ()
           | _ -> raise (VMError "Type error: if requires boolean"))
      | MakeClosure (param, func_code, self_name) ->
          (match self_name with
           | Some name ->
               let rec self = VClosure ((name, self) :: !env, param, func_code, self_name) in
               push self
           | None ->
               push (VClosure (!env, param, func_code, self_name)))
      | Call ->
          (match pop () with
           | VClosure (closure_env, param, func_code, _) ->
               let arg = pop () in
               call_stack := (!pc, !stack, !env) :: !call_stack;
               pc := 0;
               stack := [];
               env := (param, arg) :: closure_env;
               execute_func func_code
           | _ -> raise (VMError "Type error: call requires function"))
      | Return ->
          (match !call_stack with
           | (old_pc, old_stack, old_env) :: rest ->
               let result =
                 match !stack with
                 | [v] -> v
                 | [] -> VUnit
                 | v :: _ -> v
               in
               pc := old_pc;
               stack := old_stack;
               env := old_env;
               call_stack := rest;
               push result
           | [] -> pc := Array.length code)
      | MakeList n ->
          let rec loop acc n =
            if n = 0 then acc
            else loop (pop () :: acc) (n - 1)
          in
          push (VList (loop [] n))
      | Cons ->
          (match pop (), pop () with
           | VList tail, head -> push (VList (head :: tail))
           | _ -> raise (VMError "Type error: :: requires a list"))
      | Head ->
          (match pop () with
           | VList (h :: _) -> push h
           | VList [] -> raise (VMError "head: empty list")
           | _ -> raise (VMError "head: expected list"))
      | Tail ->
          (match pop () with
           | VList (_ :: t) -> push (VList t)
           | VList [] -> raise (VMError "tail: empty list")
           | _ -> raise (VMError "tail: expected list"))
      | Length ->
          (match pop () with
           | VList l -> push (VInt (List.length l))
           | VString s -> push (VInt (String.length s))
           | _ -> raise (VMError "length: expected list or string"))
      | Concat ->
          (match pop (), pop () with
           | VString b, VString a -> push (VString (a ^ b))
           | _ -> raise (VMError "Type error: ^ requires strings"))
      | Print ->
          let v = pop () in
          print_endline (string_of_vm_value v);
          push VUnit
      | Pop -> ignore (pop ())
      | Dup ->
          match !stack with
          | v :: _ -> push v
          | [] -> raise (VMError "Stack underflow")
    done
  
  and execute_func func_code =
    while !pc < Array.length func_code do
      let instr = func_code.(!pc) in
      pc := !pc + 1;
      
      match instr with
      | Return ->
          (match !call_stack with
           | (old_pc, old_stack, old_env) :: rest ->
               let result =
                 match !stack with
                 | [v] -> v
                 | [] -> VUnit
                 | v :: _ -> v
               in
               pc := old_pc;
               stack := old_stack;
               env := old_env;
               call_stack := rest;
               push result
           | [] -> pc := Array.length func_code)
      | PushInt n -> push (VInt n)
      | PushBool b -> push (VBool b)
      | PushString s -> push (VString s)
      | PushUnit -> push VUnit
      | PushNil -> push VNil
      | LoadVar x -> push (lookup !env x)
      | StoreVar x -> env := (x, pop ()) :: !env
      | Add ->
          (match pop (), pop () with
           | VInt b, VInt a -> push (VInt (a + b))
           | _ -> raise (VMError "Type error: + requires integers"))
      | Sub ->
          (match pop (), pop () with
           | VInt b, VInt a -> push (VInt (a - b))
           | _ -> raise (VMError "Type error: - requires integers"))
      | Mul ->
          (match pop (), pop () with
           | VInt b, VInt a -> push (VInt (a * b))
           | _ -> raise (VMError "Type error: * requires integers"))
      | Div ->
          (match pop (), pop () with
           | VInt 0, VInt _ -> raise (VMError "Division by zero")
           | VInt b, VInt a -> push (VInt (a / b))
           | _ -> raise (VMError "Type error: / requires integers"))
      | Eq ->
          (match pop (), pop () with
           | VInt b, VInt a -> push (VBool (a = b))
           | VBool b, VBool a -> push (VBool (a = b))
           | VString b, VString a -> push (VBool (a = b))
           | _ -> push (VBool false))
      | Neq ->
          (match pop (), pop () with
           | VInt b, VInt a -> push (VBool (a <> b))
           | VBool b, VBool a -> push (VBool (a <> b))
           | VString b, VString a -> push (VBool (a <> b))
           | _ -> push (VBool true))
      | Lt ->
          (match pop (), pop () with
           | VInt b, VInt a -> push (VBool (a < b))
           | _ -> raise (VMError "Type error: < requires integers"))
      | Le ->
          (match pop (), pop () with
           | VInt b, VInt a -> push (VBool (a <= b))
           | _ -> raise (VMError "Type error: <= requires integers"))
      | Gt ->
          (match pop (), pop () with
           | VInt b, VInt a -> push (VBool (a > b))
           | _ -> raise (VMError "Type error: > requires integers"))
      | Ge ->
          (match pop (), pop () with
           | VInt b, VInt a -> push (VBool (a >= b))
           | _ -> raise (VMError "Type error: >= requires integers"))
      | And ->
          (match pop (), pop () with
           | VBool b, VBool a -> push (VBool (a && b))
           | _ -> raise (VMError "Type error: && requires booleans"))
      | Or ->
          (match pop (), pop () with
           | VBool b, VBool a -> push (VBool (a || b))
           | _ -> raise (VMError "Type error: || requires booleans"))
      | Not ->
          (match pop () with
           | VBool b -> push (VBool (not b))
           | _ -> raise (VMError "Type error: not requires boolean"))
      | Jump addr -> pc := addr
      | JumpIfFalse addr ->
          (match pop () with
           | VBool false -> pc := addr
           | VBool true -> ()
           | _ -> raise (VMError "Type error: if requires boolean"))
      | MakeClosure (param, func_code, self_name) ->
          (match self_name with
           | Some name ->
               let rec self = VClosure ((name, self) :: !env, param, func_code, self_name) in
               push self
           | None ->
               push (VClosure (!env, param, func_code, self_name)))
      | Call ->
          (match pop () with
           | VClosure (closure_env, param, func_code, _) ->
               let arg = pop () in
               call_stack := (!pc, !stack, !env) :: !call_stack;
               pc := 0;
               stack := [];
               env := (param, arg) :: closure_env;
               execute_func func_code
           | _ -> raise (VMError "Type error: call requires function"))
      | MakeList n ->
          let rec loop acc n =
            if n = 0 then acc
            else loop (pop () :: acc) (n - 1)
          in
          push (VList (loop [] n))
      | Cons ->
          (match pop (), pop () with
           | VList tail, head -> push (VList (head :: tail))
           | _ -> raise (VMError "Type error: :: requires a list"))
      | Head ->
          (match pop () with
           | VList (h :: _) -> push h
           | VList [] -> raise (VMError "head: empty list")
           | _ -> raise (VMError "head: expected list"))
      | Tail ->
          (match pop () with
           | VList (_ :: t) -> push (VList t)
           | VList [] -> raise (VMError "tail: empty list")
           | _ -> raise (VMError "tail: expected list"))
      | Length ->
          (match pop () with
           | VList l -> push (VInt (List.length l))
           | VString s -> push (VInt (String.length s))
           | _ -> raise (VMError "length: expected list or string"))
      | Concat ->
          (match pop (), pop () with
           | VString b, VString a -> push (VString (a ^ b))
           | _ -> raise (VMError "Type error: ^ requires strings"))
      | Print ->
          let v = pop () in
          print_endline (string_of_vm_value v);
          push VUnit
      | Pop -> ignore (pop ())
      | Dup ->
          match !stack with
          | v :: _ -> push v
          | [] -> raise (VMError "Stack underflow")
    done
  in
  
  execute ();
  
  match !stack with
  | [v] -> v
  | [] -> VUnit
  | v :: _ -> v
