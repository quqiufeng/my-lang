.PHONY: all build test clean run repl example doc

# 默认目标
all: build

# 构建项目
build:
	eval $$(opam env) && dune build

# 运行测试
test:
	eval $$(opam env) && dune test

# 运行 REPL
repl:
	eval $$(opam env) && dune exec my_lang

# 运行示例
example:
	@echo "可用示例:"
	@echo "  make run-arithmetic"
	@echo "  make run-functions"
	@echo "  make run-recursion"
	@echo "  make run-lists"
	@echo "  make run-quicksort"

run-%:
	eval $$(opam env) && dune exec my_lang -- examples/language/$*.ml 2>/dev/null || \
	eval $$(opam env) && dune exec my_lang -- examples/advanced/$*.ml 2>/dev/null || \
	eval $$(opam env) && dune exec my_lang -- examples/stdlib/$*.ml 2>/dev/null

# 清理构建产物
clean:
	dune clean

# 格式化代码
format:
	eval $$(opam env) && dune build @fmt --auto-promote

# 查看文档
doc:
	@echo "文档位置:"
	@echo "  README.md              - 项目入口"
	@echo "  docs/ARCHITECTURE.md   - 架构设计"
	@echo "  docs/TUTORIAL.md       - 实现教程"
	@echo "  docs/CONTRIBUTING.md   - 扩展指南"
	@echo "  docs/BEST_PRACTICES.md - 最佳实践"
