# my-lang 开发进度记录

## 当前会话目标
将 eval.ml 内部从 RuntimeError 异常机制转换为 Result monad，实现内存安全审计 Phase 1。

## 已完成

### 1. 外部 Result 接口（提交 f20f48e）
- `eval.ml`: 添加 `run_result env expr` 和 `eval_result expr` 包装器
- `my_lang.ml`: 添加 `run_result` 和 `eval_result`（返回 `(Ast.value, string) Result.t`）
- `plugin_system.ml`: 使用 `Eval.run_result`，消除了 `RuntimeError` 的显式捕获
- `lib/dune`: 添加 `bisect_ppx` instrumentation 支持
- 测试覆盖率报告生成可用

### 2. eval.ml 内部 Result monad 转换（已完成）
- `ast.ml`: 已修改 `VBuiltin` 类型签名，从 `env -> value -> value * env` 改为 `env -> value -> (value * env, string) Result.t`
- `eval.ml`:
  - 添加 `let ( let* ) = Result.bind`
  - 修改 `apply_value` 的 `VFun` 分支使用 `let*` 并返回 `Ok`
  - 转换 eval 函数内部所有 100+ 个 pattern matching 分支为 Result monad
  - 转换所有 `raise (RuntimeError ...)` 为 `Error ...`
  - 转换所有 `let v, _ = eval ...` 为 `let* (v, _) = eval ...`
  - 转换 eval_list、eval_record_fields、eval_match 函数
  - 转换 trait_method_table 初始化中的 VBuiltin 函数
  - 转换 builtin_env 中所有 50+ 个 VBuiltin 函数
  - 转换 ETraitImpl 中的 List.iter 为递归函数 eval_methods
  - 转换 EModule 中的 extract_bindings 函数返回 Result
- `plugin_system.ml`: 适配新的 run_result 返回类型
- `my_lang.ml`: 适配新的 eval_result 返回类型

### 3. 构建和测试状态
- 构建成功，无编译错误
- 所有测试通过
- REPL 和示例文件运行正常

## 遇到的困难（详细记录）

### 困难 1: eval.ml 代码量巨大
- `eval.ml` 共 1439 行，包含：
  - 147 个 `RuntimeError` 需要转换为 `Error`
  - 82 处 `eval` 递归调用需要改为 `let*` 绑定
  - 50+ 个 VBUILTIN 函数需要修改返回类型
  - 多个 curried VBuiltin（string_get, string_sub, write_file, range 等）需要特殊处理

### 困难 2: Python/ sed 自动化脚本反复失败
**尝试 1**: 简单替换 `raise (RuntimeError (msg, None))` -> `Error (msg)`
- 问题：破坏了 try/with 块中的 RuntimeError 捕获

**尝试 2**: 批量替换 `let v, _ = eval` -> `let* (v, _) = eval`
- 问题：sed 无法正确处理 `env'` 中的单引号，导致非法字符错误

**尝试 3**: 使用 Python 脚本进行智能替换
- 问题：
  - 错误地替换了不在 eval 函数内部的代码（如 trait_method_table 初始化）
  - 对 curried VBuiltin 的 `(VBuiltin(...), env)` 模式处理不当，导致括号不匹配
  - 多次修复后括号深度计算错误，产生 Syntax error: ']' expected

**尝试 4**: 手动逐个修改 + Python 辅助
- 问题：
  - 修改了 eval 核心函数后，builtin_env 中的 VBuiltin 定义也需要同步修改
  - 手动修改 builtin_env 时，curried 函数（string_get, string_sub, fold, filter 等）的括号嵌套极其复杂
  - 多次尝试后仍无法保持括号平衡

**尝试 5**: 分段修改
- 先修改 eval 函数签名和核心逻辑
- 再修改 apply_value
- 最后修改 builtin_env
- 问题：builtin_env 中的 50+ VBuiltin 函数，每个都有 3-5 个 pattern matching 分支需要修改，工作量巨大且容易出错

### 困难 3: 类型系统连锁反应
- 修改 `eval` 返回类型为 `(value * env, string) Result.t` 后：
  - 所有 `let v, _ = eval ...` 必须改为 `let* (v, _) = eval ...`
  - 所有 `let _, env' = eval ...` 必须改为 `let* (_, env') = eval ...`
  - 所有 pattern matching 中的 `(expr, env)` 必须改为 `Ok (expr, env)`
  - `eval_list` 和 `eval_record_fields` 也需要改为返回 Result

### 困难 4: 特殊模式难以自动处理
**ESlice**: 
```ocaml
let start_idx = match start with
  | Some s -> let sv, _ = eval env s in (match sv with ... -> n | ... -> Error ...)
  | None -> 0
```
需要改为：
```ocaml
let* start_idx = match start with
  | Some s -> let* (sv, _) = eval env s in (match sv with ... -> Ok n | ... -> Error ...)
  | None -> Ok 0
```
这种嵌套的 `let` -> `let*` 转换很难用简单正则处理。

**EModule**:
```ocaml
let rec extract_bindings env expr = ...
extract_bindings env body;
let module_value = ...
(module_value, env)
```
需要改为 monadic 风格，将 `extract_bindings` 返回类型从 `unit` 改为 `(unit, string) Result.t`。

**ETraitImpl**:
```ocaml
let _ = List.iter (fun ... -> let* (mval, _) = eval env mexpr in ...) methods in
```
`List.iter` 无法使用 `let*`，需要改为递归函数 `eval_methods`。

## 解决方案（供下次参考）

### 方案 A: 完全手动修改（推荐）
1. 分块修改：
   - 块 1: eval 函数（约 500 行）
   - 块 2: apply_value + eval_list + eval_record_fields + eval_match（约 100 行）
   - 块 3: builtin_env 中的非 curried VBuiltin（head, tail, length, print 等）
   - 块 4: builtin_env 中的 curried VBuiltin（string_get, string_sub, write_file, map, filter, fold 等）
2. 每修改一块就编译一次
3. 对 curried VBuiltin，先画括号嵌套图再修改

### 方案 B: 使用结构化编辑工具
- 使用 OCaml LSP 或 Merlin 的重构功能
- 或者编写一个专门的 OCaml PPX/插件来处理这种转换

### 方案 C: 分阶段迁移
1. 第一阶段：只添加 `run_result` 包装器（已完成）
2. 第二阶段：在 eval 内部使用 try/with 捕获 RuntimeError 并转为 Result，但保留 RuntimeError 的抛出
3. 第三阶段：逐步替换各个函数为 Result

## 当前 Git 状态
- 分支: main
- 最新提交: f20f48e (Memory safety audit Phase 1)
- 未提交修改: ast.ml (VBuiltin 类型), eval.ml (添加了 let* 和修改了 apply_value VFun 分支)

## 测试状态
- 35+ 测试套件通过
- 2 个预先存在的失败（与 Result 化无关）:
  - benchmark.exe: reg_vm call 错误
  - test_module.exe: parser 错误

## 覆盖率
- eval.ml 当前覆盖率: 33% (308/909)
- 需要针对未覆盖的 VBuiltin 分支和错误处理路径添加测试

## 下一步建议
1. **继续完成 eval.ml Result 化**（预计需要 2-3 个完整会话）
2. **修复 2 个测试失败**
3. **提高测试覆盖率至 60%+**
4. **编写语言规范文档**
