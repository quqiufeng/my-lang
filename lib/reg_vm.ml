open Core
open Reg_bytecode

exception RegVMError of string

type frame = {
    ret_pc : int;
    ret_dst : int;
    ret_func : int;
    saved_regs : reg_value array;
    saved_env : (string * reg_value) list;
}

let execute prog =
    let exception Done of reg_value in
    
    let call_stack = ref [] in
    let current_func_idx = ref prog.entry_point in
    let regs = ref [||] in
    let env = ref [] in
    let pc = ref 0 in
    let switch_func = ref false in
    
    let get_reg r = !regs.(r) in
    let set_reg r v = !regs.(r) <- v in
    
    let lookup_env x =
        match List.Assoc.find !env ~equal:String.equal x with
        | Some v -> v
        | None -> raise (RegVMError ("未绑定变量: " ^ x))
    in
    
    let set_env x v =
        env := List.Assoc.add !env ~equal:String.equal x v
    in
    
    let init_func idx args closure_env ~reuse_regs =
        current_func_idx := idx;
        let func = prog.functions.(idx) in
        let new_len = func.max_regs + func.num_locals in
        if reuse_regs && new_len <= Array.length !regs then
            Array.fill !regs ~pos:0 ~len:(Array.length !regs) (RVInt 0)
        else
            regs := Array.create ~len:new_len (RVInt 0);
        env := closure_env;
        List.iteri args ~f:(fun i arg ->
            !regs.(i) <- arg;
            if i < List.length func.params then
                env := List.Assoc.add !env ~equal:String.equal (List.nth_exn func.params i) arg
        );
        pc := 0;
    in
    
    init_func prog.entry_point [] [] ~reuse_regs:false;
    
    try
        while true do
            let func = prog.functions.(!current_func_idx) in
            let code = func.code in
            
            while !pc < Array.length code && not !switch_func do
                let instr = code.(!pc) in
                pc := !pc + 1;
                
                match instr with
                | RLoadConst (d, c) ->
                    (match func.constants.(c) with
                     | CPInt n -> set_reg d (RVInt n)
                     | CPBool b -> set_reg d (RVBool b)
                     | CPString s -> set_reg d (RVString s)
                     | CPUnit -> set_reg d RVUnit)
                
                | RLoadNil d -> set_reg d RVNil
                
                | RMove (d, s) -> set_reg d (get_reg s)
                
                | RAdd (d, s1, s2) ->
                    (match get_reg s1, get_reg s2 with
                     | RVInt a, RVInt b -> set_reg d (RVInt (a + b))
                     | RVString a, RVString b -> set_reg d (RVString (a ^ b))
                     | _ -> raise (RegVMError "add: 类型错误"))
                
                | RSub (d, s1, s2) ->
                    (match get_reg s1, get_reg s2 with
                     | RVInt a, RVInt b -> set_reg d (RVInt (a - b))
                     | _ -> raise (RegVMError "sub: 需要整数"))
                
                | RMul (d, s1, s2) ->
                    (match get_reg s1, get_reg s2 with
                     | RVInt a, RVInt b -> set_reg d (RVInt (a * b))
                     | _ -> raise (RegVMError "mul: 需要整数"))
                
                | RDiv (d, s1, s2) ->
                    (match get_reg s1, get_reg s2 with
                     | RVInt _, RVInt 0 -> raise (RegVMError "除零错误")
                     | RVInt a, RVInt b -> set_reg d (RVInt (a / b))
                     | _ -> raise (RegVMError "div: 需要整数"))
                
                | REq (d, s1, s2) ->
                    (match get_reg s1, get_reg s2 with
                     | RVInt a, RVInt b -> set_reg d (RVBool (Int.equal a b))
                     | RVBool a, RVBool b -> set_reg d (RVBool (Bool.equal a b))
                     | RVString a, RVString b -> set_reg d (RVBool (String.equal a b))
                     | _ -> raise (RegVMError "eq: 类型不匹配"))
                
                | RNeq (d, s1, s2) ->
                    (match get_reg s1, get_reg s2 with
                     | RVInt a, RVInt b -> set_reg d (RVBool (not (Int.equal a b)))
                     | RVBool a, RVBool b -> set_reg d (RVBool (not (Bool.equal a b)))
                     | _ -> raise (RegVMError "neq: 类型不匹配"))
                
                | RLt (d, s1, s2) ->
                    (match get_reg s1, get_reg s2 with
                     | RVInt a, RVInt b -> set_reg d (RVBool (a < b))
                     | _ -> raise (RegVMError "lt: 需要整数"))
                
                | RLe (d, s1, s2) ->
                    (match get_reg s1, get_reg s2 with
                     | RVInt a, RVInt b -> set_reg d (RVBool (a <= b))
                     | _ -> raise (RegVMError "le: 需要整数"))
                
                | RGt (d, s1, s2) ->
                    (match get_reg s1, get_reg s2 with
                     | RVInt a, RVInt b -> set_reg d (RVBool (a > b))
                     | _ -> raise (RegVMError "gt: 需要整数"))
                
                | RGe (d, s1, s2) ->
                    (match get_reg s1, get_reg s2 with
                     | RVInt a, RVInt b -> set_reg d (RVBool (a >= b))
                     | _ -> raise (RegVMError "ge: 需要整数"))
                
                | RAnd (d, s1, s2) ->
                    (match get_reg s1, get_reg s2 with
                     | RVBool a, RVBool b -> set_reg d (RVBool (a && b))
                     | _ -> raise (RegVMError "and: 需要布尔值"))
                
                | ROr (d, s1, s2) ->
                    (match get_reg s1, get_reg s2 with
                     | RVBool a, RVBool b -> set_reg d (RVBool (a || b))
                     | _ -> raise (RegVMError "or: 需要布尔值"))
                
                | RNot (d, s) ->
                    (match get_reg s with
                     | RVBool b -> set_reg d (RVBool (not b))
                     | _ -> raise (RegVMError "not: 需要布尔值"))
                
                | RLoadVar (d, name) -> set_reg d (lookup_env name)
                
                | RStoreVar (name, s) -> set_env name (get_reg s)
                
                | RJump offset -> pc := !pc + offset - 1
                
                | RJumpIfFalse (c, offset) ->
                    (match get_reg c with
                     | RVBool false -> pc := !pc + offset - 1
                     | RVBool true -> ()
                     | _ -> raise (RegVMError "jump_if_false: 需要布尔值"))
                
                | RJumpIfTrue (c, offset) ->
                    (match get_reg c with
                     | RVBool true -> pc := !pc + offset - 1
                     | RVBool false -> ()
                     | _ -> raise (RegVMError "jump_if_true: 需要布尔值"))
                
                | RLoadFunc (d, f) ->
                    let cl = RVClosure (f, !env) in
                    
                    set_reg d cl
                
                | RCall (d, f, arg_regs) ->
                    let args = List.map arg_regs ~f:get_reg in
                    (match get_reg f with
                     | RVClosure (f_idx, closure_env) ->
                         call_stack := {
                             ret_pc = !pc;
                             ret_dst = d;
                             ret_func = !current_func_idx;
                             saved_regs = !regs;
                             saved_env = !env;
                         } :: !call_stack;
                         init_func f_idx args closure_env ~reuse_regs:false;
                         switch_func := true
                     | _ -> raise (RegVMError "call: 不是函数"))
                
                | RTailCall (f, arg_regs) ->
                    let args = List.map arg_regs ~f:get_reg in
                    (match get_reg f with
                     | RVClosure (f_idx, closure_env) ->
                         init_func f_idx args closure_env ~reuse_regs:true;
                         switch_func := true
                     | _ -> raise (RegVMError "tail_call: 不是函数"))
                
                | RReturn r ->
                    let result = get_reg r in
                    (match !call_stack with
                     | frame :: rest ->
                         current_func_idx := frame.ret_func;
                         regs := frame.saved_regs;
                         env := frame.saved_env;
                         pc := frame.ret_pc;
                         frame.saved_regs.(frame.ret_dst) <- result;
                         call_stack := rest;
                         switch_func := true
                     | [] -> raise (Done result))
                
                | RMakeRef (d, s) ->
                    let r = RVRef (ref (get_reg s)) in
                    
                    set_reg d r
                
                | RDeref (d, r) ->
                    (match get_reg r with
                     | RVRef rv -> set_reg d !rv
                     | _ -> raise (RegVMError "deref: 不是引用"))
                
                | RAssignRef (r, v) ->
                    (match get_reg r with
                     | RVRef rv -> rv := get_reg v
                     | _ -> raise (RegVMError "assign_ref: 不是引用"))
                
                | RMakeList (d, elem_regs) ->
                    let elems = List.map elem_regs ~f:get_reg in
                    let lst = RVList elems in
                    
                    set_reg d lst
                
                | RMakeTuple (d, elem_regs) ->
                    let elems = List.map elem_regs ~f:get_reg in
                    let tup = RVTuple elems in
                    
                    set_reg d tup
                
                | RListGet (d, l, i) ->
                    (match get_reg l, get_reg i with
                     | RVList elems, RVInt idx ->
                         (match Array_ops.list_nth_opt elems idx with
                          | Some v -> set_reg d v
                          | None -> raise (RegVMError "list_get: 索引越界"))
                     | _ -> raise (RegVMError "list_get: 需要列表和整数索引"))
                
                | RListLen (d, l) ->
                    (match get_reg l with
                     | RVList elems -> set_reg d (RVInt (List.length elems))
                     | _ -> raise (RegVMError "list_len: 需要列表"))
                
                | RPrint r ->
                    (match get_reg r with
                     | RVInt n -> print_endline (string_of_int n)
                     | RVBool b -> print_endline (string_of_bool b)
                     | RVString s -> print_endline s
                     | RVUnit -> print_endline "()"
                     | RVNil -> print_endline "nil"
                     | RVRef r -> print_endline ("ref " ^ string_of_int (match !r with RVInt n -> n | _ -> 0))
                     | RVList elems -> print_endline ("[" ^ String.concat ~sep:"; " (List.map elems ~f:(fun v ->
                         match v with RVInt n -> string_of_int n | RVBool b -> string_of_bool b | RVString s -> "\"" ^ s ^ "\"" | _ -> "?")) ^ "]")
                     | RVTuple elems -> print_endline ("(" ^ String.concat ~sep:", " (List.map elems ~f:(fun v ->
                         match v with RVInt n -> string_of_int n | RVBool b -> string_of_bool b | RVString s -> "\"" ^ s ^ "\"" | _ -> "?")) ^ ")")
                     | RVClosure _ -> print_endline "<closure>")
                
                | RNop -> ()
                
                | _ -> raise (RegVMError ("未实现指令: " ^ string_of_reg_instr instr))
            done;
            
            switch_func := false;
            
            (* 重新获取当前函数，因为 RReturn 可能修改了 current_func_idx *)
            let func = prog.functions.(!current_func_idx) in
            let code = func.code in
            
            (* 函数自然结束：返回 Unit *)
            if !pc >= Array.length code then (
                match !call_stack with
                | frame :: rest ->
                    current_func_idx := frame.ret_func;
                    regs := frame.saved_regs;
                    env := frame.saved_env;
                    pc := frame.ret_pc;
                    frame.saved_regs.(frame.ret_dst) <- RVUnit;
                    call_stack := rest
                | [] -> raise (Done RVUnit)
            )
        done;
        RVUnit
    with Done v -> v
