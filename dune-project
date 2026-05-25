(lang dune 3.23)
(using menhir 3.0)

(name my_lang)

(generate_opam_files true)

(source
 (github quqiufeng/my-lang))

(authors "Your Name <your.email@example.com>")

(maintainers "Your Name <your.email@example.com>")

(license MIT)

(documentation https://github.com/quqiufeng/my-lang)

(package
 (name my_lang)
 (synopsis "一个用 OCaml 实现的简单函数式编程语言")
 (description "MyLang 是一个教学用编程语言实现，展示了如何用 OCaml 构建一门完整的函数式语言。包含词法分析、语法分析、类型推断、字节码编译器和虚拟机。")
 (depends
  ocaml
  dune
  core
  menhir)
 (tags
  ("programming-language" "compiler" "interpreter" "functional-programming" "ocaml" "tutorial")))
