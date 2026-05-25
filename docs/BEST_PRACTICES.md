# MyLang 开发最佳实践

> 本文档总结了 MyLang 开发过程中的关键经验教训和最佳实践，
> 涵盖 VM 设计、编译器开发、调试技巧和代码质量维护。

---

## 1. VM 设计：避免代码重复

### 问题
当 VM 需要支持函数调用时，最初的实现将主代码执行和函数体执行分成两个几乎完全相同的函数（`execute` 和 `execute_func`），导致约 **250 行重复代码**。

### 解决方案
提取统一的 `exec_instr` 函数处理单条指令，用 `execute_block` 执行代码块：

```ocaml
let rec exec_instr instr =
  match instr with
  | PushInt n -> push (VInt n)
  | LoadVar x -> push (lookup !env x)
  | Call -> (* 递归调用 execute_block *)
  | Return -> (* 恢复调用者状态或抛出 ReturnExn *)
  | ...

and execute_block block_code =
  while !pc < Array.length block_code do
    let instr = block_code.(!pc) in
    pc := !pc + 1;
    exec_instr instr
  done
```

### 原则
- **单一职责**：指令执行逻辑只写一次
- **递归调用**：`Call` 指令通过递归调用 `execute_block` 进入函数体
- **异常控制流**：使用局部异常（`let exception ReturnExn`）安全跳出嵌套循环

---

## 2. 调试技巧：VM 执行跟踪

### 方法
在 `exec_instr` 入口处添加调试输出，打印当前状态：

```ocaml
let rec exec_instr instr =
  Printf.printf "DEBUG pc=%d instr=%s env=[" 
    !pc (Bytecode.string_of_instr instr);
  List.iter (fun (k,_) -> Printf.printf "%s;" k) !env;
  Printf.printf "]\n";
  match instr with ...
```

### 关键观察点
- **环境变化**：`StoreVar` 后检查变量是否正确绑定
- **调用栈**：`Call` 时确认参数和闭包环境
- **返回点**：`Return` 时验证调用栈恢复是否正确

### 原则
- 调试输出应可快速启用/禁用
- 关注环境（env）和栈（stack）的对应关系
- 递归函数调用是 bug 高发区，需逐层验证

---

## 3. 常见 Bug 模式：Return 指令处理

### 问题
当递归函数返回时，如果最外层 `execute_block` 的 `pc` 未被正确终止，
VM 会继续执行主代码中的后续指令，而此时环境已被恢复，导致 **Unbound variable** 错误。

### 错误场景
```
factorial 5 -> 返回 120 到主函数
主函数继续执行 pc=6 (Jump 13) 
但此时 env=[factorial]（没有 n）
pc=7 (LoadVar n) -> 报错：Unbound variable: n
```

### 解决方案
使用局部异常跳出当前代码块：

```ocaml
let exception ReturnExn in

let rec exec_instr instr =
  match instr with
  | Return ->
      (match !call_stack with
       | (old_pc, old_stack, old_env) :: rest ->
           (* 恢复调用者状态 *)
           push result
       | [] ->
           (* 最外层返回：抛出异常 *)
           raise ReturnExn)
  | ...

and execute_block block_code =
  try
    while !pc < Array.length block_code do
      ...
    done
  with ReturnExn -> ()
```

### 原则
- **区分返回层级**：函数内部返回 vs 最外层返回
- **避免手动修改 pc**：不要通过 `pc := Array.length code` 来退出循环
- **使用异常**：局部异常是跳出嵌套循环的安全方式

---

## 4. 编译器开发：明确错误处理

### 问题
编译器对不支持的表达式或模式使用 `_ -> emit ctx PushUnit`，导致：
- 静默忽略错误
- 运行时行为不可预期
- 调试困难

### 解决方案
对未实现的功能使用显式 `failwith`：

```ocaml
let rec compile_expr ctx expr =
  match expr with
  | EInt n -> emit ctx (PushInt n)
  | EMatch (e, cases) -> compile_match ctx e cases
  | _ -> failwith "compile_expr: unsupported expression"

and compile_match ctx e cases =
  match cases with
  | (PInt n, body) :: rest -> ...
  | (PBool _, _) :: _ ->
      failwith "compile_match: boolean patterns not yet supported in bytecode"
  | (PCons _, _) :: _ ->
      failwith "compile_match: cons patterns not yet supported in bytecode"
```

### 原则
- **fail fast**：在编译期捕获错误，而非运行时
- **清晰错误信息**：包含位置（函数名）和原因
- **区分层级**：解释器支持 ≠ 字节码支持，需分别标注

---

## 5. 注释规范

### 模块级文档
每个 `.ml` 文件开头应有模块级文档注释：

```ocaml
(** 模块功能概述

    简要说明模块的职责和主要数据结构。
    
    核心概念：
    - 概念1：解释...
    - 概念2：解释...
*)
```

### 函数文档
关键函数应包含：
- **功能说明**：函数做什么
- **参数说明**：每个参数的语义
- **返回值**：返回什么
- **副作用**：是否修改全局状态
- **异常**：可能抛出的异常

```ocaml
(** 应用替换到类型

    [apply subst t] 将类型 [t] 中所有出现在替换域中的类型变量
    替换为对应类型。
    
    时间复杂度：O(|t| * log|subst|)
*)
let rec apply subst t = ...
```

### 内联注释
- 复杂算法步骤需逐行注释
- 控制流（if/match/loop）注释分支意图
- 调试代码标记 `(* DEBUG: ... *)`

---

## 6. 测试策略

### 测试分层
```
解释器测试（test_my_lang.ml）
├── 基础类型：int, bool, string
├── 复合类型：list, tuple
├── 控制流：if, match
├── 函数：lambda, let rec
└── 错误处理：类型错误, 运行时错误

字节码测试（test_bytecode.ml）
├── 编译正确性
├── 执行正确性
└── 与解释器结果一致性
```

### 关键测试用例
- **递归函数**：验证调用栈和环境的正确保存/恢复
- **模式匹配**：验证各分支类型一致性
- **错误场景**：除零、未绑定变量、类型不匹配

### 测试驱动修复
1. 发现 bug 时先编写最小复现测试
2. 运行测试确认失败
3. 修复代码
4. 运行全部测试确认无回归

---

## 7. 性能优化原则

### 已验证的优化
| 优化点 | 原实现 | 优化后 | 影响 |
|--------|--------|--------|------|
| 类型替换 | (int * t) list | Int.Map | O(n) → O(log n) |
| 指令累积 | Array.append | list 累积 + 反转 | O(n²) → O(n) |
| VM 状态 | 纯函数 | mutable refs | 减少 GC 压力 |

### 优化原则
- **先测量，后优化**：确保优化有实际效果
- **保持正确性**：优化后必须跑通全部测试
- **局部化变更**：一次只改一个模块，便于回滚

---

## 8. Git 工作流

### 提交规范
```
类型：简短描述

- 详细变更1
- 详细变更2
- 详细变更3
```

类型包括：
- **feat**: 新功能
- **fix**: Bug 修复
- **perf**: 性能优化
- **refactor**: 重构
- **docs**: 文档
- **test**: 测试

### 提交前检查清单
- [ ] `dune build` 无错误无警告
- [ ] `dune test` 全部通过
- [ ] 变更文件已审查（`git diff --stat`）
- [ ] 无调试代码残留

---

## 9. 常见陷阱

### OCaml 特有
- **`let exception` 作用域**：局部异常必须在 `let ... in` 中定义
- **`ref` 的不可变性**：`ref` 本身不可变，其内容可变，注意不要重新绑定 ref
- **列表反转**：`emit` 使用 `instr :: code` 累积，最终需 `List.rev`

### 编译器/VM 交互
- **AST 修改影响**：修改 `Ast.expr` 需同步更新 parser、compiler、eval、typeinfer
- **字节码一致性**：新增指令需同步更新 bytecode.ml、compiler.ml、vm.ml
- **递归闭包**：`let rec` 闭包需通过 `let rec self = VClosure (...)` 实现自引用

---

## 10. 代码实现本地验证方法与要求

### 验证目标
确保每次代码变更后，项目能在本地正确构建并通过全部测试（当前 85 个测试用例）。

### 环境准备
```bash
# 激活 OCaml opam 环境（必须执行，否则找不到依赖）
eval $(opam env)

# 验证环境
ocaml --version
dune --version
```

### 验证流程

#### 第一步：构建检查
```bash
dune build
```

**通过标准**：
- 零错误（Error）
- 零警告（Warning），特别是：
  - `partial-match`：模式匹配不完整（如新增 `VArray`/`VRecord` 后未在 `type_of_vm_value` 中处理）
  - `unused-var`：未使用变量
  - `deprecated`：废弃函数调用

**常见错误及修复**：
| 错误类型 | 示例 | 修复方法 |
|---------|------|---------|
| 模式匹配不完整 | `Warning 8: partial-match` | 补全所有构造函数分支 |
| 未绑定变量 | `Unbound variable: n` | 检查变量名拼写或环境绑定 |
| 类型不匹配 | `expected type vm_value` | 确认 AST/VM/Compiler 类型定义一致 |

#### 第二步：测试验证
```bash
dune test
```

**通过标准**：
- 全部 85 个测试通过
- 无失败（Failure）、无错误（Error）、无跳过（Skip）

**测试覆盖范围**：
- 解释器测试（test_my_lang.ml）：基础类型、复合类型、控制流、函数、错误处理
- 字节码测试（test_bytecode.ml）：编译正确性、执行正确性、与解释器一致性

#### 第三步：快速回归验证
修改关键模块（AST、Parser、Compiler、VM）后，必须执行：
```bash
eval $(opam env) && dune build && dune test
```

### 验证失败处理流程

1. **构建失败**：
   - 查看第一个错误位置
   - 检查依赖模块（如修改 AST 需同步更新 parser/compiler/eval/typeinfer）

2. **测试失败**：
   - 定位失败测试用例
   - 最小化复现：提取失败代码片段单独测试
   - 对比解释器和字节码执行结果是否一致

3. **警告处理**：
   - 严禁使用 `_` 通配符忽略未处理分支（除非是 truly unreachable）
   - 新增类型构造函数后，必须同步更新所有模式匹配位置

### 示例：完整验证会话
```bash
$ eval $(opam env)
$ cd /home/quqiufeng/my-lang

$ dune build
（无输出 = 成功）

$ dune test
（显示测试进度和结果）
File "test/test_my_lang.ml", line 1, characters 0-0:
        test alias test/runtest
（全部通过）
```

### 关键原则
- **每次提交前必做**：`dune build && dune test`
- **零容忍警告**：OCaml 的警告往往是隐藏 bug
- **修改 AST/Bytecode 需全链路更新**：parser → compiler → vm → typeinfer → eval
- **新增功能必有测试**：参考现有测试模式，在 `test/test_my_lang.ml` 中添加用例

---

## 附录 A：添加 while 循环的完整流程

### 1. AST (`lib/ast.ml`)
```ocaml
type expr =
  | ...
  | EWhile of expr * expr   (* 条件 * 循环体 *)
```

### 2. Lexer (`lib/lexer.mll`)
```ocaml
| "while"  { WHILE }
| "do"     { DO }
| "done"   { DONE }
```

### 3. Parser (`lib/parser.mly`)
```ocaml
%token WHILE DO DONE

expr:
  | WHILE c = expr DO body = expr DONE { EWhile (c, body) }
```

### 4. Type Checker (`lib/typeinfer.ml`)
```ocaml
| EWhile (cond, body) ->
    let tc = infer env cond in
    let _ = infer env body in
    unify_ref tc TBool;
    TUnit
```

### 5. Evaluator (`lib/eval.ml`)
```ocaml
| EWhile (cond, body) ->
    let rec loop env =
      let v, _ = eval env cond in
      match v with
      | VBool true ->
          let _, env' = eval env body in
          loop env'
      | VBool false -> (VUnit, env)
      | _ -> raise (RuntimeError "while requires boolean condition")
    in
    loop env
```

### 6. Compiler (`lib/compiler.ml`)
```ocaml
| EWhile (cond, body) ->
    let loop_pos = code_length ctx in
    compile_expr ctx cond;
    let jump_end_pos = code_length ctx in
    emit ctx (JumpIfFalse 0);
    compile_expr ctx body;
    emit ctx Pop;
    emit ctx (Jump loop_pos);
    let end_pos = code_length ctx in
    patch_instr ctx jump_end_pos (JumpIfFalse end_pos);
    emit ctx PushUnit
```

### 7. VM (`lib/vm.ml`)
已支持 Jump/JumpIfFalse，无需修改。

---

## 附录 B：添加索引访问的完整流程

### 1. AST (`lib/ast.ml`)
```ocaml
type expr =
  | ...
  | EIndex of expr * expr   (* 被索引对象 * 索引值 *)
```

### 2. Parser (`lib/parser.mly`)
```ocaml
simple_expr:
  | e = simple_expr LBRACKET idx = expr RBRACKET { EIndex (e, idx) }
```

### 3. Type Checker (`lib/typeinfer.ml`)
```ocaml
| EIndex (e1, e2) ->
    let t1 = infer env e1 in
    let t2 = infer env e2 in
    let t_elem = new_var () in
    unify_ref t2 TInt;
    (match apply_current t1 with
     | TList _ -> unify_ref t1 (TList t_elem); apply_current t_elem
     | TString -> TString
     | _ -> unify_ref t1 (TList t_elem); apply_current t_elem)
```

### 4. Evaluator (`lib/eval.ml`)
```ocaml
| EIndex (e1, e2) ->
    let v1, _ = eval env e1 in
    let v2, _ = eval env e2 in
    (match v1, v2 with
     | VList vs, VInt idx when idx >= 0 && idx < List.length vs ->
         (List.nth vs idx, env)
     | VString s, VInt idx when idx >= 0 && idx < String.length s ->
         (VString (String.make 1 s.[idx]), env)
     | _ -> raise (RuntimeError "index out of bounds"))
```

### 5. Bytecode (`lib/bytecode.ml`)
```ocaml
type instr =
  | ...
  | Index    (* 从栈顶弹出索引和列表/字符串，压入元素 *)
```

### 6. Compiler (`lib/compiler.ml`)
```ocaml
| EIndex (e1, e2) ->
    compile_expr ctx e1;
    compile_expr ctx e2;
    emit ctx Index
```

### 7. VM (`lib/vm.ml`)
```ocaml
| Index ->
    (match pop (), pop () with
     | VInt idx, VList vs ->
         if idx < 0 || idx >= List.length vs then
           raise (VMError "index out of bounds")
         else push (List.nth vs idx)
     | VInt idx, VString s ->
         if idx < 0 || idx >= String.length s then
           raise (VMError "string index out of bounds")
         else push (VString (String.make 1 s.[idx]))
     | _ -> raise (VMError "index requires int and list/string"))
```

---

## 附录 C：尾调用优化 (TCO) 实现指南

### 问题
递归函数调用增长调用栈，导致栈溢出：
```ocaml
let rec sum = fun n -> fun acc ->
  if n = 0 then acc else sum (n - 1) (acc + n)
in sum 100000 0   (* 栈溢出！ *)
```

### 解决方案
**窥孔优化（Peephole Optimization）**：扫描字节码，将 `Call + Return` 替换为 `TailCall`。

#### 1. 添加 TailCall 指令
```ocaml
(* lib/bytecode.ml *)
type instr =
  | ...
  | TailCall   (* 尾调用：复用当前栈帧 *)
```

#### 2. 实现窥孔优化
```ocaml
(* lib/compiler.ml *)
let optimize_tail_calls code =
  let rec loop acc = function
    | [] -> List.rev acc
    | Call :: Return :: rest -> loop (TailCall :: acc) rest
    | h :: t -> loop (h :: acc) t
  in
  loop [] code
```

#### 3. 在 get_code 中应用优化
```ocaml
let get_code_with_opt ctx =
  Array.of_list (optimize_tail_calls (List.rev ctx.code))
```

注意：**仅在函数体内部应用优化**，顶层代码不使用优化，避免影响主程序返回。

#### 4. VM 执行 TailCall
```ocaml
(* lib/vm.ml *)
| TailCall ->
    (match pop () with
     | VClosure (closure_env, param, func_code, _) ->
         let arg = pop () in
         (* 不保存调用者状态，直接复用当前栈帧 *)
         pc := 0;
         stack := [];
         env := (param, arg) :: closure_env;
         execute_block func_code
     | _ -> raise (VMError "Type error: call requires function"))
```

### 关键区别
| 指令 | 行为 |
|------|------|
| **Call** | 保存 (pc, stack, env) 到 call_stack，创建新栈帧 |
| **TailCall** | 直接覆盖 pc, stack, env，复用当前栈帧 |

### 验证
编写深递归测试验证栈不增长：
```ocaml
let rec sum = fun n -> fun acc ->
  if n = 0 then acc else sum (n - 1) (acc + n)
in sum 100000 0   (* 开启 TCO 后正常运行 *)
```

---

## 附录 D：异常处理字节码编译

### 问题
`try...with` 和 `raise` 在解释器中工作正常，但字节码编译器用 `failwith` 跳过。

### 解决方案
1. **新增指令**：`PushHandler addr`（压入异常处理程序地址）、`PopHandler`（弹出）、`RaiseExn`（抛出栈顶异常值）
2. **VM 状态**：添加 `handler_stack` 保存 `(handler地址, 当前栈, 当前环境)`
3. **RaiseExn 执行**：弹出异常值，恢复 handler 状态（pc、stack、env），将异常值压入栈并跳转
4. **编译器**：将 `try e with cases` 编译为：
   ```
   PushHandler catch_addr
   <e>
   PopHandler
   Jump end_addr
   catch_addr:
   StoreVar __exn__
   <pattern匹配>
   end_addr:
   ```

### 关键注意点
- `PushHandler` 必须在 `exec_instr` 的 `match` 中被正确解析，注意缩进和括号
- `Dup` 分支中的 `match !stack with` 必须用括号包裹，否则 OCaml 解析器可能将其解析为 `match` 的分支

## 附录 E：切片语法字节码编译

### 问题
`list[1:3]` 和 `"hello"[1:4]` 在解释器中支持，但字节码编译器不支持。

### 解决方案
1. **新增指令**：`Slice`
2. **VM 执行**：从栈弹出 `(end_idx, start_idx, list/string)`，计算切片并压入结果
3. **编译器**：将 `None` 表示为 `PushInt 0`（起始）或 `PushInt (-1)`（结束，表示到末尾）
4. **统一赋值**：`EAssign` 在编译器中根据左侧表达式类型（ref/array/record）生成不同指令

### 统一赋值策略
```ocaml
| EAssign (e1, e2) ->
    (match e1 with
     | EArrayGet (arr, idx) -> ... emit ctx ArraySet
     | ERecordGet (e, field) -> ... emit ctx (RecordSet field)
     | _ -> ... emit ctx SetRef)
```

这避免了为每种赋值类型创建单独的 AST 节点。

---

*最后更新：2026-05-25*
