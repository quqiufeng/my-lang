# MyLang - OCaml 编程语言开发框架

一个可扩展的编程语言开发框架，基于 OCaml 实现。

> **核心理念**: 像搭积木一样设计编程语言 —— 只需定义 AST 和语法，底座提供完整的编译器基础设施。

## 快速开始

```bash
git clone https://github.com/quqiufeng/my-lang.git
cd my-lang
eval $(opam env)
dune build
dune exec my_lang
```

## 功能特性

**语言特性**：函数式编程、静态类型推断、模式匹配、ADT/GADT、Traits、代数效果、Actor 并发

**后端支持**：解释器、字节码 VM、寄存器 VM、JIT x86-64、WASM

**工具链**：LSP 语言服务器、包管理器、调试器、增量编译

**标准库**：200+ 函数，覆盖字符串、列表、数学、JSON、网络、加密、并发

## 项目结构

```
my-lang/
├── lib/              # 核心库
│   ├── ast.ml        # 抽象语法树
│   ├── parser.mly    # 语法分析器
│   ├── eval.ml       # 求值器
│   ├── compiler.ml   # 字节码编译器
│   └── vm.ml         # 虚拟机
├── framework/        # 语言开发框架
├── templates/        # 语言模板
├── test/             # 测试
├── examples/         # 示例
└── docs/             # 文档
```

## 创建新语言

```bash
cp -r templates/basic_language my_language
cd my_language
dune build
dune exec bin/main.exe
```

只需实现 AST、词法、语法、求值器，即可获得完整的语言工具链。

## 文档

- [最佳实践](docs/BEST_PRACTICES.md) - 开发经验总结
- [教程](docs/TUTORIAL.md) - 从零创建语言
- [架构](docs/ARCHITECTURE.md) - 系统设计
- [标准库](docs/STDLIB.md) - API 参考
- [路线图](docs/ROADMAP.md) - 开发计划

## 为什么选择 OCaml？

- **代数数据类型**：天然适合表示 AST
- **模式匹配**：编译器代码简洁易读
- **类型推断**：确保代码正确性
- **成功案例**：Rust、Coq、F* 都用 OCaml 开发

## License

MIT
