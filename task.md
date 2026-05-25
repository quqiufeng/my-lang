# my-lang 开发任务跟踪

**最后更新**: 2026-05-26
**当前测试数**: 95 个（全部通过）
**代码行数**: ~8000+ 行
**阶段**: 阶段 4（工具链）已完成

---

## ✅ 已完成

### 核心基础设施
- [x] Hindley-Milner 类型推断（多态类型）
- [x] 字节码编译器 + 虚拟机（两阶段执行）
- [x] AST 解释器（eval）
- [x] REPL 交互式环境
- [x] 自动化测试框架（95 个测试）

### 语言特性
- [x] **ADT（代数数据类型）** — `type color = Red | Green | Blue`
- [x] **泛型 ADT** — `type 'a option = None | Some of 'a`
- [x] **引用类型** — `ref 42`, `!x`, `x := 20`
- [x] **异常处理** — `try expr with | Pattern -> handler`, `raise expr`
- [x] **数组类型** — `[|1, 2, 3|]`, `a.(0)`, `a.(0) <- 42`
- [x] **字符类型** — `'a'`, `'\n'`
- [x] **字符串操作** — `string_length`, `string_get`, `string_sub`
- [x] **文件 IO** — `read_file`, `write_file`, `read_line`, `print_string`
- [x] **记录类型** — `{name = "x"; age = 1}`, `p.name`, `p.name <- "y"`
- [x] **记录更新** — `{p with name = "new"}`
- [x] **模块系统** — `module M = struct ... end`, `open M`, `M.x`
- [x] **类型标注** — `let x : int = 42`
- [x] **语法糖** — `assert`, `ignore`, `|>` 管道, `..` 范围, `todo`

### 编译器后端
- [x] 字节码编译器 + VM（31+ 指令）
- [x] 尾调用优化（TCO）
- [x] 异常处理字节码编译（PushHandler/PopHandler/RaiseExn）
- [x] 切片字节码编译
- [x] 元组/列表模式匹配字节码编译
- [x] 嵌套模式匹配字节码编译
- [x] 解释器与字节码一致性验证（30 个测试）
- [x] WASM 后端（内存分配、列表、字符串、构造函数）
- [x] 垃圾回收器（mark-sweep）

### 工程化
- [x] 模块导入
- [x] 高阶函数（map/filter/fold）
- [x] 负整数与二元减法解析修复
- [x] 标准库（Map、Set、Queue、Stack、Option、Result）
- [x] 解析器冲突修复（0 reduce/reduce）

### 工具链
- [x] **包管理器** — `my-lang.toml`, `init`, `build`, `install`, `test`
- [x] **LSP 语言服务器** — 代码补全、hover 提示、错误诊断

---

## 项目结构

```
my-lang/
├── bin/              # CLI / REPL
├── lib/              # 核心库
│   ├── ast.ml        # 抽象语法树
│   ├── lexer.mll     # 词法分析器
│   ├── parser.mly    # 语法分析器
│   ├── eval.ml       # 树遍历解释器
│   ├── typeinfer.ml  # Hindley-Milner 类型推断
│   ├── compiler.ml   # AST -> 字节码编译器
│   ├── vm.ml         # 字节码虚拟机
│   ├── gc.ml         # 垃圾回收器
│   ├── wasm_backend.ml # WASM 文本生成
│   ├── package_manager.ml # 包管理器
│   ├── lsp_server.ml # LSP 语言服务器
│   └── my_lang.ml    # 库入口
├── test/             # 测试套件
├── examples/         # 示例程序
└── docs/             # 文档
```

---

## CLI 命令

```bash
# REPL
my_lang

# 运行文件
my_lang file.ml

# 编译
my_lang compile file.ml
my_lang compile --wasm file.ml

# 包管理
my_lang init project-name
my_lang build
my_lang install
my_lang test
my_lang info

# LSP 服务器
my_lang lsp
```

---

## 🚧 未来工作（可选）

### 性能优化
- [ ] JIT 编译
- [ ] 增量编译
- [ ] 常量折叠
- [ ] 死代码消除

### 类型系统扩展
- [ ] 类型类/Traits（类似 Rust）
- [ ] GADT（广义代数数据类型）
- [ ] 效果系统（Algebraic Effects）

### 标准库扩展
- [ ] Unicode 字符串支持
- [ ] 完整文件 IO
- [ ] 网络库（HTTP/TCP）
- [ ] JSON 解析/生成
- [ ] 正则表达式

### 并发
- [ ] 轻量级线程
- [ ] 通道（Channel）
- [ ] Actor 模型

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
| 泛型 ADT | 3 | ✅ |
| 记录更新 | 3 | ✅ |
| 模块系统 | 3 | ✅ |
| 一致性验证 | 30 | ✅ |
| **总计** | **95** | **✅** |

---

## 🔑 关键决策记录

1. **ADT 类型参数**: 存储为 `string option` 避免 AST↔Types 循环依赖
2. **引用类型**: 使用 OCaml `ref`（`'a ref`）实现，非 GC 管理
3. **异常**: 使用 OCaml 异常 `Exception_value of value` 传播
4. **数组**: 使用 OCaml `Array` 实现，O(1) 索引
5. **字节码**: 已支持数组/异常/记录更新/引用/切片/元组/嵌套模式匹配
6. **中文错误**: 统一使用 `type_of_value` / `type_of_vm_value` 助手
7. **测试策略**: 每次变更后必须 95 个测试全部通过
8. **记录类型**: 使用可变字段 `value ref`，支持 `p.field <- value` 赋值
9. **模块系统**: EModule 创建 VModule，EDot 访问模块字段
10. **包管理器**: 简单 TOML 解析，支持 init/build/install/test
11. **LSP 服务器**: JSON-RPC 协议，支持 completion/hover/diagnostics

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
| 模块系统 | 已完成 | ✅ |
| WASM 后端 | 已完成 | ✅ |
| 垃圾回收 | 已完成 | ✅ |
| 包管理器 | 已完成 | ✅ |
| LSP 服务器 | 已完成 | ✅ |
| **工业级语言** | **目标: 1 年** | 🚧 |

---

*本文档每次开发会话后更新。记得在变更后运行 `dune test`。*
