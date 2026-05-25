# my-lang 开发任务跟踪

**最后更新**: 2026-05-25
**当前测试数**: 85 个（全部通过）
**代码行数**: ~5000+ 行
**阶段**: 阶段 1（基础语言完备）接近完成

---

## ✅ 已完成

### 核心基础设施
- [x] Hindley-Milner 类型推断（多态类型）
- [x] 字节码编译器 + 虚拟机（两阶段执行）
- [x] AST 解释器（eval）
- [x] REPL 交互式环境
- [x] 自动化测试框架（76 个测试）

### 语言特性
- [x] **ADT（代数数据类型）** — `type color = Red | Green | Blue`
  - 值: `VCtor`, 模式: `PCtor`, 表达式: `ECtor`, `ETypeDef`
  - 类型: `TADT`
  - 字节码: `PushCtor`, `TestCtor`, `GetCtorArg`
  - 测试: 5 个
  
- [x] **引用类型** — `ref 42`, `!x`, `x := 20`
  - 值: `VRef`, 类型: `TRef`
  - 字节码: `MakeRef`, `Deref`, `SetRef`
  - 测试: 4 个
  
- [x] **异常处理** — `try expr with | Pattern -> handler`, `raise expr`
  - 值: `VExn`, 表达式: `ETry`, `ERaise`
  - 异常: `Exception_value of value`
  - 测试: 4 个
  
- [x] **数组类型** — `[|1, 2, 3|]`, `a.(0)`, `a.(0) <- 42`
  - 值: `VArray`, 类型: `TArray`
  - 表达式: `EArray`, `EArrayGet`, `EArraySet`
  - 测试: 5 个
  
- [x] **字符类型** — `'a'`, `'\n'`
  - 值: `VChar`, 类型: `TChar`, 表达式: `EChar`
  - 字节码: `PushChar`
  - 测试: 2 个
  
- [x] **字符串操作** — `string_length`, `string_get`, `string_sub`
  - 内置函数，柯里化调用
  - 测试: 4 个
  
- [x] **文件 IO** — `read_file`, `write_file`, `read_line`, `print_string`
  - 内置函数，基于 Core.In_channel/Out_channel
  - 测试: 2 个
  
- [x] **错误定位基础设施** — `pos` 类型，行列号支持
  - RuntimeError 携带位置信息
  - 为完整位置跟踪打下基础
  - 测试: 84 个全部通过
  
- [x] **记录类型** — `{name = "x"; age = 1}`, `p.name`, `p.name <- "y"`
  - 值: `VRecord`，类型: `TRecord`，表达式: `ERecord`, `ERecordGet`, `ERecordSet`
  - 可变字段（使用 `value ref`）
  - 测试: 5 个

### 已有特性（早期实现）
- [x] 基础类型: int, bool, string, unit
- [x] 复合类型: list, tuple, function
- [x] 控制流: if/then/else, while/do/done, match/with
- [x] 绑定: let, let rec, fun
- [x] 运算符: +, -, *, /, =, <, >, &&, ||, not, ::, ^
- [x] 索引: e1[e2], 切片: e[start:end]
- [x] 内置函数: head, tail, length, print, show, import, map, filter, fold
- [x] 尾调用优化（TCO 窥孔优化）
- [x] 中文错误信息
- [x] 文档: ARCHITECTURE, TUTORIAL, CONTRIBUTING, BEST_PRACTICES

---

## 🚧 进行中 / 待开始（按优先级排序）

### 🔴 阶段 1: 基础语言完备（当前阶段）

**目标**: 让语言能写真实程序（CLI 工具、数据处理脚本）

#### 1. 错误定位（位置信息） 🚧 基础设施已完成
- [x] `pos` 类型定义
- [x] RuntimeError 携带位置信息
- [x] 错误消息显示位置格式
- [ ] Lexer/Parser 传播位置到 AST（后续完善）
- [ ] 调用栈回溯（stack trace）（后续完善）
- [ ] 字节码指令映射到源码位置（后续完善）

#### 2. 字符类型与字符串操作 ✅ 已完成
- [x] `'a'` 字符字面量
- [x] `char` 类型
- [x] `string_length` / `string_get` / `string_sub`
- [ ] `String.concat` / `String.split`（延后到标准库）
- [ ] `StringBuffer`（延后）
- [ ] 字符串迭代: `String.iter`, `String.map`（延后）

#### 3. 文件 IO ✅ 已完成
- [x] `read_file : string -> string`
- [x] `write_file : string -> string -> unit`
- [x] `read_line : unit -> string`
- [x] `print_string` / `print_endline`（已有 print）
- [ ] 标准输出/错误引用（延后）

#### 4. 记录类型（Record）✅ 已完成
- [x] `{name = "x"; age = 1}` 记录字面量
- [x] `p.name` 字段访问
- [x] `p.name <- "new"` 字段修改（可变字段）
- [x] 记录模式匹配 `{name = n; age = a}`
- [x] 记录模式简写 `{name; age}`
- [x] 记录更新: `{p with name = "new"}`

#### 5. 解析器冲突修复 ✅ 已完成
- [x] 重构解析器为运算符优先级分层结构
- [x] 从 41 reduce/reduce + 37 shift/reduce 减少到 0 reduce/reduce + 9 shift/reduce
- [x] 统一赋值处理：`EAssign` 处理 ref/数组/记录赋值

#### 6. 其他语法糖 ✅ 已完成
- [x] `assert expr` 断言
- [x] `ignore expr` 显式丢弃值
- [x] 管道操作符 `|>`: `x |> f` = `f x`
- [x] 范围表达式: `1..10`
- [x] `todo "msg"` 未实现占位

### 🟡 阶段 2: 编译器与运行时强化

#### 1. 字节码完善 ✅ 已完成
- [x] 数组的字节码编译支持（MakeArray, ArrayGet, ArraySet）
- [x] 异常的字节码编译支持（PushHandler, PopHandler, RaiseExn）
- [x] 引用的字节码编译验证（MakeRef, Deref, SetRef）
- [x] 记录的字节码编译支持（MakeRecord, RecordGet, RecordSet）
- [x] 切片的字节码编译支持（Slice）
- [x] 元组的字节码编译支持（MakeTuple）
- [x] 嵌套模式匹配字节码编译（compile_pattern_test）
- [x] 解释器与字节码一致性验证（28 个测试全部通过）
- [x] 解析器负整数修复（`-1` 作为 INT token）

#### 2. 字节码优化（可选）
- [ ] 常量折叠
- [ ] 死代码消除
- [ ] 指令选择优化

#### 2. 垃圾回收（GC）
- [ ] 分代式 GC 设计
- [ ] 新生代（复制算法）
- [ ] 老年代（标记-清除/整理）
- [ ] 根集扫描
- [ ] 增量标记（减少停顿）

#### 3. 执行后端
- [ ] WASM 后端: 字节码 -> WASM
- [ ] AOT 编译: `my-lang build` 生成可执行文件
- [ ] LLVM 后端（可选）

#### 4. 并发模型
- [ ] 轻量级线程（goroutine-style）
- [ ] 通道（Channel）
- [ ] 或 Actor 模型
- [ ] 或 STM（软件事务内存）

### 🟢 阶段 3: 模块与类型系统

#### 1. 模块系统
- [ ] `module Foo = { ... }`
- [ ] `open Foo`
- [ ] 签名（Signatures）
- [ ] 函子（Functors）
- [ ] 可见性控制（public/private）

#### 2. 高级类型
- [ ] 泛型 ADT: `type 'a option = Some of 'a | None`
- [ ] 类型类/Traits（类似 Rust）
- [ ] GADT（广义代数数据类型）
- [ ] 效果系统（Algebraic Effects）

### 🔵 阶段 4: 工具链

#### 1. 包管理器
- [ ] `my-lang new project`
- [ ] `my-lang.toml` 配置
- [ ] `my-lang add package`
- [ ] `my-lang build` / `my-lang test`
- [ ] 包注册表

#### 2. 开发工具
- [ ] LSP 语言服务器
- [ ] 调试器（断点、单步）
- [ ] 格式化工具 `my-lang fmt`
- [ ] 文档生成 `my-lang doc`

### 🟣 阶段 5: 标准库与生态

#### 核心库
- [x] **Option/Result**: `Some`, `None`, `Ok`, `Error`（非参数化版本，标准库）
- [ ] **Map**: 基于 AVL 树或哈希表
- [ ] **Set**: 集合操作
- [ ] **Array**: 丰富操作（已有基础）
- [ ] **String**: Unicode 支持
- [ ] **Char**: 字符分类

#### IO 与系统
- [ ] **File**: 完整文件操作
- [ ] **Path**: 跨平台路径
- [ ] **Process**: 子进程
- [ ] **Env**: 环境变量
- [ ] **Time**: 日期时间

#### 网络与数据
- [ ] **Json**: 解析/生成
- [ ] **Http**: 客户端/服务器
- [ ] **Net**: TCP/UDP
- [ ] **Regex**: 正则表达式

---

## 📊 测试统计

| 类别 | 测试数 | 状态 |
|------|--------|------|
| 基础语法 | 8 | ✅ |
| 字符串 | 4 | ✅ |
| 列表 | 4 | ✅ |
| 元组 | 2 | ✅ |
| let rec | 2 | ✅ |
| 序列 | 2 | ✅ |
| 内置函数 | 4 | ✅ |
| 模式匹配 | 8 | ✅ |
| 类型错误 | 5 | ✅ |
| while 循环 | 1 | ✅ |
| 索引/切片 | 6 | ✅ |
| show | 3 | ✅ |
| 高阶函数 | 3 | ✅ |
| ADT | 5 | ✅ |
| 引用类型 | 4 | ✅ |
| 异常处理 | 4 | ✅ |
| 数组类型 | 5 | ✅ |
| 字符类型 | 2 | ✅ |
| 字符串操作 | 4 | ✅ |
| 文件 IO | 2 | ✅ |
| 记录类型 | 5 | ✅ |
| **总计** | **85** | **✅** |

---

## 🔑 关键决策记录

1. **ADT 类型参数**: 存储为 `string option` 避免 AST↔Types 循环依赖
2. **引用类型**: 使用 OCaml `ref`（`'a ref`）实现，非 GC 管理
3. **异常**: 使用 OCaml 异常 `Exception_value of value` 传播
4. **数组**: 使用 OCaml `Array` 实现，O(1) 索引
5. **字节码**: 已支持数组/异常/记录更新/引用/切片/元组/嵌套模式匹配
6. **中文错误**: 统一使用 `type_of_value` / `type_of_vm_value` 助手
7. **测试策略**: 每次变更后必须 89 个测试全部通过
8. **记录类型**: 使用可变字段 `value ref`，支持 `p.field <- value` 赋值

---

## 📝 待办事项（近期）

### 下次开发会话（建议顺序）

1. ~~**解析器冲突修复**~~ ✅ 已完成
2. ~~**字符类型**~~ ✅ 已完成
3. ~~**字符串操作**~~ ✅ 已完成
4. ~~**文件 IO**~~ ✅ 已完成
5. ~~**错误定位基础设施**~~ ✅ 已完成
6. ~~**记录类型**~~ ✅ 已完成
7. ~~**字节码编译器补全**~~ ✅ 已完成（数组/异常/记录/引用/切片/元组/嵌套模式）
8. ~~**解释器与字节码一致性验证**~~ ✅ 已完成（28/28 通过）
9. ~~**语法糖**~~ ✅ 已完成（assert, ignore, |> 管道）
10. **标准库基础** — Option, Result 类型 🚧 待开始

### 技术债务

- [x] 解析器冲突过多（41 reduce/reduce + 37 shift/reduce）✅ 已修复（0 reduce/reduce + 9 shift/reduce）
- [x] 字节码编译器需要补全数组/异常/记录/引用/切片/元组支持 ✅ 已修复
- [x] VM 的 `type_of_vm_value` 缺少 `VArray`/`VTuple` 描述 ✅ 已修复
- [x] `string_of_value` 缺少 `VArray`/`VTuple` 输出 ✅ 已修复
- [x] 元组字节码与解释器不一致（MakeList vs MakeTuple）✅ 已修复
- [x] 记录更新暂不支持字节码编译 ✅ 已修复（CopyRecord 指令）

---

## 🎯 里程碑

| 里程碑 | 日期 | 状态 |
|--------|------|------|
| 基础解释器 | 已完成 | ✅ |
| 类型推断 | 已完成 | ✅ |
| 字节码 VM | 已完成 | ✅ |
| ADT | 已完成 | ✅ |
| 引用类型 | 已完成 | ✅ |
| 异常处理 | 已完成 | ✅ |
| 数组类型 | 已完成 | ✅ |
| **能写 CLI 工具** | **目标: 2 周内** | 🚧 |
| **自举编译器** | **目标: 6 个月** | ⏳ |
| **工业级语言** | **目标: 1 年** | ⏳ |

---

*本文档每次开发会话后更新。记得在变更后运行 `make test`。*
