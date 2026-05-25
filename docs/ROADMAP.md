# my-lang 工业级路线图

## 阶段 1: 基础语言完备（当前 → 2 周）

让语言能写真实程序（>1000 行的 CLI 工具、数据处理脚本）。

### 1.1 错误定位（位置信息）
- AST 所有节点带 `pos: {line; col}`
- Lexer/Parser 传播位置
- RuntimeError / TypeError 显示 `file.ml:12:34`
- 调用栈回溯（stack trace）

### 1.2 引用与可变状态
- `ref expr` 创建引用
- `!expr` 解引用
- `expr1 := expr2` 赋值
- 类型 `t ref`
- 支持引用上的模式匹配

### 1.3 异常处理
- `try expr with | Pattern -> handler`
- 内置异常类型：`Failure of string`, `Not_found`
- `raise expr` 抛出异常
- 异常安全（finally 语义）

### 1.4 数组类型
- `[|e1; e2; e3|]` 数组字面量
- `arr.(idx)` 索引访问
- `arr.(idx) <- val` 赋值
- `Array.length`, `Array.make`, `Array.map`
- 可变长度（动态数组）

### 1.5 字符与字符串
- `'a'` 字符字面量
- `char` 类型
- `String.length`, `String.get`, `String.sub`
- `String.concat`, `String.split`
- 字符串是可变的（或提供 StringBuffer）

### 1.6 文件 IO
- `read_file : string -> string`
- `write_file : string -> string -> unit`
- `stdout`, `stderr` 作为引用/通道
- `read_line : unit -> string`
- `print_endline` / `print_string`

### 1.7 其他基础
- `assert expr` 断言
- `todo "msg"` 未实现占位
- `ignore expr` 显式丢弃值
- 管道操作符 `|>`：`x |$gt; f` = `f x`
- 范围表达式：`1..10`

---

## 阶段 2: 编译器与运行时强化（2-4 周）

### 2.1 垃圾回收
- **分代式 GC**：新生代（复制算法）+ 老年代（标记-清除/整理）
- 增量/并发 GC 标记（减少停顿）
- 弱引用（Weak references）用于缓存

### 2.2 执行后端
- **WASM 后端**：字节码 -> WASM（浏览器可运行）
- **LLVM 后端**（可选）：直接生成机器码
- **AOT 编译**：`my-lang build` 生成可执行文件

### 2.3 性能优化
- 内联缓存（Inline caches）用于动态调用
- 逃逸分析（Escape analysis）栈分配
- 常量折叠、死代码消除
- 性能分析器（Profiler）：`my-lang profile`

### 2.4 并发模型
- **轻量级线程**（M:N 线程，类似 Go goroutine / Erlang process）
- 消息传递（Actor model）：`spawn`, `send`, `receive`
- 或 **结构化并发**（Structured concurrency）
- 或 **软件事务内存**（STM）

---

## 阶段 3: 模块与类型系统（3-5 周）

### 3.1 模块系统
```my-lang
module String = {
  type t = string
  let length = ...
  let map = ...
}

open String
let s = map (fun c -> ...) "hello"
```
- 签名（Signatures）：接口定义
- 函子（Functors）：模块参数化
- 模块递归

### 3.2 抽象类型与封装
- `type t`（抽象类型）
- `type t = private ...`（私有类型）
- 模块可见性控制（public / private）

### 3.3 高级类型特性
- **记录类型**：`type person = {name: string; age: int}`
- **行多态**（Row polymorphism）：开放记录
- **类型类/Traits**（类似 Rust/Haskell）：
  ```my-lang
  trait Show {
    show : self -> string
  }
  
  impl Show for int {
    show = fun n -> string_of_int n
  }
  ```
- **GADT**（广义代数数据类型）：`type 'a expr = Int : int -> int expr | Add : int expr * int expr -> int expr`
- **依赖类型**（可选研究特性）：`type vec (n: nat) = ...`

### 3.4 效果系统（Algebraic Effects）
```my-lang
effect State {
  get : unit -> int
  set : int -> unit
}

let counter = fun init ->
  let state = ref init in
  handle {
    get () = !state
    set n = state := n
  } in
  ...
```

---

## 阶段 4: 工具链（4-6 周）

### 4.1 包管理器
- `my-lang new project` 创建项目
- `my-lang.toml` 配置（依赖、版本、作者）
- `my-lang add package` 安装依赖
- `my-lang build` 构建
- `my-lang test` 运行测试
- `my-lang publish` 发布到注册表
- 注册表：GitHub 仓库或自建服务器

### 4.2 LSP 语言服务器
- 自动补全（基于类型和作用域）
- 类型提示（Hover 显示类型）
- 跳转到定义
- 查找引用
- 重命名重构
- 代码操作（自动生成 match 分支）

### 4.3 调试器
- 源码级调试（断点、单步、查看变量）
- 支持字节码和 WASM 后端
- `my-lang debug script.ml`

### 4.4 格式化工具
- `my-lang fmt` 自动格式化
- 可配置风格（缩进、行宽）

### 4.5 文档生成
- `my-lang doc` 从注释生成 API 文档
- Markdown 支持
- 类型化文档（类似 Rustdoc）

---

## 阶段 5: 标准库与生态（持续）

### 5.1 核心标准库
- **Prelude**：自动导入的基础函数
- **Option/Result**：错误处理
- **List**：不可变链表（已有）
- **Array**：可变数组
- **Map**：基于树/哈希的映射（`Map.make`, `Map.get`, `Map.set`）
- **Set**：集合操作
- **String**：Unicode 感知字符串处理
- **Char**：字符操作
- **Int/Float**：数值运算
- **Bool**：逻辑运算
- **Tuple**：元组工具

### 5.2 IO 与系统
- **File**：文件操作（读/写/追加/删除）
- **Path**：跨平台路径处理
- **Process**：子进程管理
- **Env**：环境变量
- **Time**：日期时间、计时器
- **Random**：伪随机数

### 5.3 并发与网络
- **Task**：异步任务/期约（Promise）
- **Channel**：并发通道
- **Net**：TCP/UDP/HTTP 客户端
- **Http**：HTTP 服务器（类似 Go net/http）
- **Json**：JSON 解析/生成
- **Xml**：XML 处理

### 5.4 测试框架
- `assert_eq`, `assert_true`
- 属性测试（Property-based testing，类似 QuickCheck）
- 基准测试（Benchmark）
- 覆盖率报告

### 5.5 正则表达式
- `Regex.match`, `Regex.replace`
- 或内置模式匹配语法扩展

---

## 时间估算

| 阶段 | 全职工作量 | 关键交付物 |
|------|-----------|-----------|
| 阶段 1 | 2 周 | 能写 CLI 工具和数据处理脚本 |
| 阶段 2 | 3 周 | WASM 输出、GC、goroutine |
| 阶段 3 | 4 周 | 模块系统、Traits、GADT |
| 阶段 4 | 4 周 | 包管理器、LSP、调试器 |
| 阶段 5 | 持续 | 丰富的标准库和生态 |

**总计：3 个月达到 "可用"，6 个月达到 "好用"，1 年达到 "工业级"。**

---

## 当前状态（2026-05-25）

### ✅ 已完成
- ADT（代数数据类型）
- 引用类型（ref/!/:=）
- 异常处理（try/raise）
- 数组类型（[| |]/.()/.()<-）
- 字符类型（'a', '\n'）
- 字符串操作（string_length, string_get, string_sub）
- 文件 IO（read_file, write_file, read_line, print_string）
- 错误定位基础设施（pos类型 + RuntimeError携带位置）
- 记录类型（{name = "x"; age = 1}, p.name, p.name <- "y"）
- 解析器冲突修复（41→0 reduce/reduce, 37→9 shift/reduce）
- **85 个测试全部通过**

### 🚧 阶段 1 剩余工作（按优先级）
1. **字节码编译器补全** — 数组/异常/记录 🚧 当前
2. **其他语法糖** — assert, |>, ignore（可选）
3. **位置信息完善** — Lexer/Parser传播位置到AST

### ⏳ 后续阶段
- 阶段 2: GC + WASM + 并发
- 阶段 3: 模块系统 + 类型类
- 阶段 4: 包管理器 + LSP
- 阶段 5: 标准库

---

## 立即开始：阶段 1 实施计划（更新版）

按优先级排序：

1. ~~**引用类型**（ref/!/:=）~~ ✅ 已完成
2. ~~**异常处理**（try/raise）~~ ✅ 已完成
3. ~~**数组**（[| |]）~~ ✅ 已完成
4. ~~**字符类型**~~ ✅ 已完成
5. ~~**字符串操作**~~ ✅ 已完成
6. ~~**文件 IO**~~ ✅ 已完成
7. ~~**错误定位基础设施**~~ ✅ 已完成
8. ~~**记录类型**~~ ✅ 已完成
9. **其他语法糖** — assert, `|>`, ignore（可选）

阶段 1 核心功能基本完成！
