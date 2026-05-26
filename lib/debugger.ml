(** 调试器 — 寄存器 VM 级别的交互式调试 *)

open Core
open Reg_bytecode

exception DebuggerError of string

type frame = {
    ret_pc : int;
    ret_dst : int;
    ret_func : int;
    saved_regs : reg_value array;
    saved_env : (string * reg_value) list;
}

type vm_state = {
    prog : reg_program;
    mutable call_stack : frame list;
    mutable current_func_idx : int;
    mutable regs : reg_value array;
    mutable env : (string * reg_value) list;
    mutable pc : int;
    mutable switch_func : bool;
}

type debug_state = {
    vm : vm_state;
    mutable breakpoints : (int * int, unit) Hashtbl.t;  (* (func_idx, pc) *)
    mutable paused : bool;
    mutable result : reg_value option;
}

let make_vm_state prog =
    let func = prog.functions.(prog.entry_point) in
    let new_len = func.max_regs + func.num_locals in
    {
        prog;
        call_stack = [];
        current_func_idx = prog.entry_point;
        regs = Array.create ~len:new_len (RVInt 0);
        env = [];
        pc = 0;
        switch_func = false;
    }

let init_debug_state prog =
    let vm = make_vm_state prog in
    {
        vm;
        breakpoints = Hashtbl.Poly.create ();
        paused = false;
        result = None;
    }

let get_reg state r = state.vm.regs.(r)
let set_reg state r v = state.vm.regs.(r) <- v

let lookup_env state x =
    match List.Assoc.find state.vm.env ~equal:String.equal x with
    | Some v -> v
    | None -> raise (Reg_vm.RegVMError ("未绑定变量: " ^ x))

let set_env state x v =
    state.vm.env <- List.Assoc.add state.vm.env ~equal:String.equal x v

let init_func state idx args closure_env ~reuse_regs =
    state.vm.current_func_idx <- idx;
    let func = state.vm.prog.functions.(idx) in
    let new_len = func.max_regs + func.num_locals in
    if reuse_regs && new_len <= Array.length state.vm.regs then
        Array.fill state.vm.regs ~pos:0 ~len:(Array.length state.vm.regs) (RVInt 0)
    else
        state.vm.regs <- Array.create ~len:new_len (RVInt 0);
    state.vm.env <- closure_env;
    List.iteri args ~f:(fun i arg ->
        state.vm.regs.(i) <- arg;
        if i < List.length func.params then
            state.vm.env <- List.Assoc.add state.vm.env ~equal:String.equal (List.nth_exn func.params i) arg
    );
    state.vm.pc <- 0

let set_breakpoint state func_idx pc =
    Base.Hashtbl.Poly.set state.breakpoints ~key:(func_idx, pc) ~data:()

let remove_breakpoint state func_idx pc =
    Base.Hashtbl.Poly.remove state.breakpoints (func_idx, pc)

let is_breakpoint state func_idx pc =
    Base.Hashtbl.Poly.mem state.breakpoints (func_idx, pc)

let get_current_location state =
    let func = state.vm.prog.functions.(state.vm.current_func_idx) in
    (func.name, state.vm.current_func_idx, state.vm.pc)

let get_stack_trace state =
    let current = (state.vm.prog.functions.(state.vm.current_func_idx).name, state.vm.pc) in
    current :: List.map state.vm.call_stack ~f:(fun f ->
        (state.vm.prog.functions.(f.ret_func).name, f.ret_pc)
    )

let get_variables state =
    state.vm.env

let get_registers state =
    Array.to_list state.vm.regs

let string_of_location state =
    let (name, idx, pc) = get_current_location state in
    Printf.sprintf "%s (func=%d, pc=%d)" name idx pc

(** 单步执行一条指令，返回 (是否继续运行, 是否遇到断点) *)
let step state =
    if state.paused then (false, false)
    else
        let vm = state.vm in
        let func = vm.prog.functions.(vm.current_func_idx) in
        let code = func.code in
        
        if vm.pc >= Array.length code then (
            (* 函数自然结束 *)
            match vm.call_stack with
            | frame :: rest ->
                vm.current_func_idx <- frame.ret_func;
                vm.regs <- frame.saved_regs;
                vm.env <- frame.saved_env;
                vm.pc <- frame.ret_pc;
                frame.saved_regs.(frame.ret_dst) <- RVUnit;
                vm.call_stack <- rest;
                (true, false)
            | [] ->
                state.result <- Some RVUnit;
                (false, false)
        ) else (
            let instr = code.(vm.pc) in
            let at_breakpoint = is_breakpoint state vm.current_func_idx vm.pc in
            vm.pc <- vm.pc + 1;
            
            (match instr with
            | RLoadConst (d, c) ->
                (match func.constants.(c) with
                 | CPInt n -> set_reg state d (RVInt n)
                 | CPBool b -> set_reg state d (RVBool b)
                 | CPString s -> set_reg state d (RVString s)
                 | CPUnit -> set_reg state d RVUnit)
            
            | RLoadNil d -> set_reg state d RVNil
            
            | RMove (d, s) -> set_reg state d (get_reg state s)
            
            | RAdd (d, s1, s2) ->
                (match get_reg state s1, get_reg state s2 with
                 | RVInt a, RVInt b -> set_reg state d (RVInt (a + b))
                 | RVString a, RVString b -> set_reg state d (RVString (a ^ b))
                 | _ -> raise (Reg_vm.RegVMError "add: 类型错误"))
            
            | RSub (d, s1, s2) ->
                (match get_reg state s1, get_reg state s2 with
                 | RVInt a, RVInt b -> set_reg state d (RVInt (a - b))
                 | _ -> raise (Reg_vm.RegVMError "sub: 需要整数"))
            
            | RMul (d, s1, s2) ->
                (match get_reg state s1, get_reg state s2 with
                 | RVInt a, RVInt b -> set_reg state d (RVInt (a * b))
                 | _ -> raise (Reg_vm.RegVMError "mul: 需要整数"))
            
            | RDiv (d, s1, s2) ->
                (match get_reg state s1, get_reg state s2 with
                 | RVInt _, RVInt 0 -> raise (Reg_vm.RegVMError "除零错误")
                 | RVInt a, RVInt b -> set_reg state d (RVInt (a / b))
                 | _ -> raise (Reg_vm.RegVMError "div: 需要整数"))
            
            | RMod (d, s1, s2) ->
                (match get_reg state s1, get_reg state s2 with
                 | RVInt a, RVInt b -> set_reg state d (RVInt (a mod b))
                 | _ -> raise (Reg_vm.RegVMError "mod: 需要整数"))
            
            | REq (d, s1, s2) ->
                (match get_reg state s1, get_reg state s2 with
                 | RVInt a, RVInt b -> set_reg state d (RVBool (Int.equal a b))
                 | RVBool a, RVBool b -> set_reg state d (RVBool (Bool.equal a b))
                 | RVString a, RVString b -> set_reg state d (RVBool (String.equal a b))
                 | _ -> raise (Reg_vm.RegVMError "eq: 类型不匹配"))
            
            | RNeq (d, s1, s2) ->
                (match get_reg state s1, get_reg state s2 with
                 | RVInt a, RVInt b -> set_reg state d (RVBool (not (Int.equal a b)))
                 | RVBool a, RVBool b -> set_reg state d (RVBool (not (Bool.equal a b)))
                 | _ -> raise (Reg_vm.RegVMError "neq: 类型不匹配"))
            
            | RLt (d, s1, s2) ->
                (match get_reg state s1, get_reg state s2 with
                 | RVInt a, RVInt b -> set_reg state d (RVBool (a < b))
                 | _ -> raise (Reg_vm.RegVMError "lt: 需要整数"))
            
            | RLe (d, s1, s2) ->
                (match get_reg state s1, get_reg state s2 with
                 | RVInt a, RVInt b -> set_reg state d (RVBool (a <= b))
                 | _ -> raise (Reg_vm.RegVMError "le: 需要整数"))
            
            | RGt (d, s1, s2) ->
                (match get_reg state s1, get_reg state s2 with
                 | RVInt a, RVInt b -> set_reg state d (RVBool (a > b))
                 | _ -> raise (Reg_vm.RegVMError "gt: 需要整数"))
            
            | RGe (d, s1, s2) ->
                (match get_reg state s1, get_reg state s2 with
                 | RVInt a, RVInt b -> set_reg state d (RVBool (a >= b))
                 | _ -> raise (Reg_vm.RegVMError "ge: 需要整数"))
            
            | RAnd (d, s1, s2) ->
                (match get_reg state s1, get_reg state s2 with
                 | RVBool a, RVBool b -> set_reg state d (RVBool (a && b))
                 | _ -> raise (Reg_vm.RegVMError "and: 需要布尔值"))
            
            | ROr (d, s1, s2) ->
                (match get_reg state s1, get_reg state s2 with
                 | RVBool a, RVBool b -> set_reg state d (RVBool (a || b))
                 | _ -> raise (Reg_vm.RegVMError "or: 需要布尔值"))
            
            | RNot (d, s) ->
                (match get_reg state s with
                 | RVBool b -> set_reg state d (RVBool (not b))
                 | _ -> raise (Reg_vm.RegVMError "not: 需要布尔值"))
            
            | RLoadVar (d, name) -> set_reg state d (lookup_env state name)
            
            | RStoreVar (name, s) -> set_env state name (get_reg state s)
            
            | RJump offset -> vm.pc <- vm.pc + offset - 1
            
            | RJumpIfFalse (c, offset) ->
                (match get_reg state c with
                 | RVBool false -> vm.pc <- vm.pc + offset - 1
                 | RVBool true -> ()
                 | _ -> raise (Reg_vm.RegVMError "jump_if_false: 需要布尔值"))
            
            | RJumpIfTrue (c, offset) ->
                (match get_reg state c with
                 | RVBool true -> vm.pc <- vm.pc + offset - 1
                 | RVBool false -> ()
                 | _ -> raise (Reg_vm.RegVMError "jump_if_true: 需要布尔值"))
            
            | RLoadFunc (d, f) ->
                let cl = RVClosure (f, vm.env) in
                set_reg state d cl
            
            | RCall (d, f, arg_regs) ->
                let args = List.map arg_regs ~f:(get_reg state) in
                (match get_reg state f with
                 | RVClosure (f_idx, closure_env) ->
                     vm.call_stack <- {
                         ret_pc = vm.pc;
                         ret_dst = d;
                         ret_func = vm.current_func_idx;
                         saved_regs = vm.regs;
                         saved_env = vm.env;
                     } :: vm.call_stack;
                     init_func state f_idx args closure_env ~reuse_regs:false;
                     vm.switch_func <- true
                 | _ -> raise (Reg_vm.RegVMError "call: 不是函数"))
            
            | RTailCall (f, arg_regs) ->
                let args = List.map arg_regs ~f:(get_reg state) in
                (match get_reg state f with
                 | RVClosure (f_idx, closure_env) ->
                     init_func state f_idx args closure_env ~reuse_regs:true;
                     vm.switch_func <- true
                 | _ -> raise (Reg_vm.RegVMError "tail_call: 不是函数"))
            
            | RReturn r ->
                let result = get_reg state r in
                (match vm.call_stack with
                 | frame :: rest ->
                     vm.current_func_idx <- frame.ret_func;
                     vm.regs <- frame.saved_regs;
                     vm.env <- frame.saved_env;
                     vm.pc <- frame.ret_pc;
                     frame.saved_regs.(frame.ret_dst) <- result;
                     vm.call_stack <- rest;
                     vm.switch_func <- true
                 | [] ->
                     state.result <- Some result;
                     vm.switch_func <- true)
            
            | RMakeRef (d, s) ->
                set_reg state d (RVRef (ref (get_reg state s)))
            
            | RDeref (d, r) ->
                (match get_reg state r with
                 | RVRef rv -> set_reg state d !rv
                 | _ -> raise (Reg_vm.RegVMError "deref: 不是引用"))
            
            | RAssignRef (r, v) ->
                (match get_reg state r with
                 | RVRef rv -> rv := get_reg state v
                 | _ -> raise (Reg_vm.RegVMError "assign_ref: 不是引用"))
            
            | RMakeList (d, elem_regs) ->
                let elems = List.map elem_regs ~f:(get_reg state) in
                set_reg state d (RVList elems)
            
            | RMakeTuple (d, elem_regs) ->
                let elems = List.map elem_regs ~f:(get_reg state) in
                set_reg state d (RVTuple elems)
            
            | RListGet (d, l, i) ->
                (match get_reg state l, get_reg state i with
                 | RVList elems, RVInt idx ->
                     (match List.nth elems idx with
                      | Some v -> set_reg state d v
                      | None -> raise (Reg_vm.RegVMError "list_get: 索引越界"))
                 | _ -> raise (Reg_vm.RegVMError "list_get: 需要列表和整数索引"))
            
            | RListSet (l, i, v) ->
                (match get_reg state l, get_reg state i, get_reg state v with
                 | RVList elems, RVInt idx, val_v ->
                     let rec set_nth n = function
                       | h :: t -> if n = 0 then val_v :: t else h :: set_nth (n - 1) t
                       | [] -> raise (Reg_vm.RegVMError "list_set: 索引越界")
                     in
                     set_reg state l (RVList (set_nth idx elems))
                 | _ -> raise (Reg_vm.RegVMError "list_set: 需要列表和整数索引"))
            
            | RListLen (d, l) ->
                (match get_reg state l with
                 | RVList elems -> set_reg state d (RVInt (List.length elems))
                 | _ -> raise (Reg_vm.RegVMError "list_len: 需要列表"))
            
            | RConcat (d, s1, s2) ->
                (match get_reg state s1, get_reg state s2 with
                 | RVString a, RVString b -> set_reg state d (RVString (a ^ b))
                 | _ -> raise (Reg_vm.RegVMError "concat: 需要字符串"))
            
            | RStringLen (d, s) ->
                (match get_reg state s with
                 | RVString str -> set_reg state d (RVInt (String.length str))
                 | _ -> raise (Reg_vm.RegVMError "string_len: 需要字符串"))
            
            | RMakeClosure (d, f, captures) ->
                let captured_env = List.filter_map captures ~f:(fun (name, reg) ->
                    match List.Assoc.find vm.env ~equal:String.equal name with
                    | Some v -> Some (name, v)
                    | None -> None
                ) in
                set_reg state d (RVClosure (f, captured_env))
            
            | RPrint r ->
                (match get_reg state r with
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
            
            | RPop n ->
                for _ = 1 to n do
                    match vm.call_stack with
                    | _ :: rest -> vm.call_stack <- rest
                    | [] -> ()
                done
            
            | RNop -> ()
            );
            
            if vm.switch_func then (
                vm.switch_func <- false;
                (true, at_breakpoint)
            ) else
                (true, at_breakpoint)
        )

(** 运行直到结束或遇到断点 *)
let rec run state =
    let (cont, hit_bp) = step state in
    if not cont then (
        match state.result with
        | Some v -> v
        | None -> RVUnit
    ) else if hit_bp then (
        state.paused <- true;
        RVUnit
    ) else
        run state

(** 继续运行，直到结束、断点或下一条指令 *)
let continue state =
    state.paused <- false;
    run state

(** 单步进入：执行一条指令，遇到函数调用时进入函数内部 *)
let step_into state =
    state.paused <- false;
    let (cont, _) = step state in
    if cont then state.paused <- true;
    cont

(** 单步跳过：执行一条指令，遇到函数调用时不进入，等函数返回后再暂停 *)
let step_over state =
    state.paused <- false;
    let start_depth = List.length state.vm.call_stack in
    let rec loop () =
        let (cont, hit_bp) = step state in
        if not cont then false
        else if hit_bp then (state.paused <- true; true)
        else if List.length state.vm.call_stack <= start_depth then (
            state.paused <- true;
            true
        ) else loop ()
    in
    loop ()

(** 单步跳出：执行直到当前函数返回 *)
let step_out state =
    state.paused <- false;
    let start_depth = List.length state.vm.call_stack in
    let rec loop () =
        let (cont, hit_bp) = step state in
        if not cont then false
        else if hit_bp then (state.paused <- true; true)
        else if List.length state.vm.call_stack < start_depth then (
            state.paused <- true;
            true
        ) else loop ()
    in
    loop ()

(** 反汇编当前位置附近的指令 *)
let disassemble_current state ~(window:int) =
    let (name, func_idx, pc) = get_current_location state in
    let func = state.vm.prog.functions.(func_idx) in
    let code = func.code in
    let buf = Buffer.create 1024 in
    Buffer.add_string buf (Printf.sprintf "=== %s (func=%d) ===\n" name func_idx);
    let start = max 0 (pc - window) in
    let finish = min (Array.length code - 1) (pc + window) in
    for i = start to finish do
        let marker = if i = pc then " >>> " else "     " in
        Buffer.add_string buf (Printf.sprintf "%s%04d: %s\n" marker i (string_of_reg_instr code.(i)))
    done;
    Buffer.contents buf
