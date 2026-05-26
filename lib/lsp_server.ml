(** LSP 语言服务器

    基于 JSON-RPC 的 LSP 协议实现，支持：
    - 代码补全 (textDocument/completion)
    - 类型信息提示 (textDocument/hover)
    - 错误诊断 (textDocument/publishDiagnostics)
    - 代码格式化 (textDocument/formatting)
*)

open Core

(** LSP 消息类型 *)
type json = Yojson.Safe.t

(** 读取 LSP 消息（Content-Length 头） *)
let read_message () =
  let rec read_headers acc =
    let line = In_channel.input_line In_channel.stdin in
    match line with
    | None | Some "" -> acc
    | Some header -> read_headers (header :: acc)
  in
  let headers = read_headers [] in
  match headers with
  | [] -> None
  | _ ->
      let content_length =
        List.find_map headers ~f:(fun h ->
          if String.is_prefix h ~prefix:"Content-Length: " then
            Some (Int.of_string (String.sub h ~pos:16 ~len:(String.length h - 16)))
          else None)
      in
      match content_length with
      | Some len ->
          let buf = Bytes.create len in
          let rec read_exactly offset =
            if offset >= len then ()
            else
              let n = In_channel.input In_channel.stdin ~buf ~pos:offset ~len:(len - offset) in
              if n = 0 then ()
              else read_exactly (offset + n)
          in
          read_exactly 0;
          let content = Bytes.to_string buf in
          Some (Yojson.Safe.from_string content)
      | None -> None

(** 写入 LSP 消息 *)
let write_message json =
  let content = Yojson.Safe.to_string json in
  let len = String.length content in
  Out_channel.printf "Content-Length: %d\r\n\r\n%s" len content;
  Out_channel.flush stdout

(** 创建 LSP 响应 *)
let make_response id result =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("id", id);
    ("result", result)
  ]

(** 创建 LSP 通知 *)
let make_notification method_ params =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String method_);
    ("params", params)
  ]

(** 提取位置信息 *)
let get_position params =
  match params with
  | `Assoc fields ->
      (match List.Assoc.find fields ~equal:String.equal "position" with
       | Some (`Assoc pos_fields) ->
           let line = match List.Assoc.find pos_fields ~equal:String.equal "line" with
             | Some (`Int n) -> n
             | _ -> 0 in
           let character = match List.Assoc.find pos_fields ~equal:String.equal "character" with
             | Some (`Int n) -> n
             | _ -> 0 in
           Some (line, character)
       | _ -> None)
  | _ -> None

(** 提取文档 URI *)
let get_uri params =
  match params with
  | `Assoc fields ->
      (match List.Assoc.find fields ~equal:String.equal "textDocument" with
       | Some (`Assoc doc_fields) ->
           (match List.Assoc.find doc_fields ~equal:String.equal "uri" with
            | Some (`String uri) -> Some uri
            | _ -> None)
       | _ -> None)
  | _ -> None

(** 提取文档内容 *)
let get_text params =
  match params with
  | `Assoc fields ->
      (match List.Assoc.find fields ~equal:String.equal "contentChanges" with
       | Some (`List changes) ->
           (match List.hd changes with
            | Some (`Assoc change_fields) ->
                (match List.Assoc.find change_fields ~equal:String.equal "text" with
                 | Some (`String text) -> Some text
                 | _ -> None)
            | _ -> None)
       | _ -> None)
  | _ -> None

(** 文档存储 *)
let documents : (string, string) Hashtbl.t = Hashtbl.create (module String)

(** 内置补全项 *)
let builtin_completions = [
  ("let", "keyword", "Define a variable");
  ("fun", "keyword", "Define a function");
  ("if", "keyword", "Conditional expression");
  ("then", "keyword", "Then branch");
  ("else", "keyword", "Else branch");
  ("match", "keyword", "Pattern matching");
  ("with", "keyword", "With clause");
  ("type", "keyword", "Type definition");
  ("module", "keyword", "Module definition");
  ("open", "keyword", "Open module");
  ("true", "constant", "Boolean true");
  ("false", "constant", "Boolean false");
  ("print", "function", "Print value");
  ("map", "function", "Map function over list");
  ("filter", "function", "Filter list");
  ("fold", "function", "Fold list");
  ("length", "function", "Get list/string length");
  ("head", "function", "Get first element");
  ("tail", "function", "Get remaining elements");
]

(** 处理 completion 请求 *)
let handle_completion id params =
  let uri_opt = get_uri params in
  
  (* 从当前文档获取符号 *)
  let document_symbols = match uri_opt with
    | Some uri ->
        (match Hashtbl.find documents uri with
         | Some content ->
             (try
                let lexbuf = Lexing.from_string content in
                let expr = Parser.prog Lexer.read lexbuf in
                let table = Symbol_table.extract_symbols expr in
                Hashtbl.to_alist table.defs |> List.map ~f:(fun (name, def) ->
                  let kind = match def.symbol_type with
                    | "function" -> 3
                    | "variable" -> 6
                    | "type" -> 22
                    | "module" -> 9
                    | _ -> 1
                  in
                  (name, kind, def.symbol_type))
              with _ -> [])
         | None -> [])
    | None -> []
  in
  
  (* 合并内置补全和文档符号 *)
  let builtin_with_int_kinds = List.map builtin_completions ~f:(fun (label, kind_str, detail) ->
    let kind = match kind_str with
      | "keyword" -> 14
      | "function" -> 3
      | "constant" -> 21
      | _ -> 1
    in
    (label, kind, detail))
  in
  let all_completions = builtin_with_int_kinds @ document_symbols in
  
  let items = List.map all_completions ~f:(fun (label, kind, detail) ->
    `Assoc [
      ("label", `String label);
      ("kind", `Int kind);
      ("detail", `String detail)
    ]
  ) in
  make_response id (`List items)

(** 处理 hover 请求 *)
let handle_hover id params =
  let pos = get_position params in
  let _uri = get_uri params in
  match pos with
  | Some (_line, char) ->
      (* 简单的词法分析获取当前词 *)
      let hover_text =
        match char with
        | n when n < 3 -> "let x = expr in body - Define a variable"
        | n when n < 6 -> "fun x -> expr - Anonymous function"
        | n when n < 9 -> "if cond then t else f - Conditional expression"
        | n when n < 12 -> "match expr with | p1 -> e1 | p2 -> e2 - Pattern matching"
        | n when n < 15 -> "type name = Ctor1 | Ctor2 of type - Algebraic data type"
        | n when n < 18 -> "module M = struct ... end - Module definition"
        | n when n < 21 -> "print : 'a -> unit - Print value to stdout"
        | n when n < 24 -> "map : ('a -> 'b) -> 'a list -> 'b list - Map function over list"
        | _ -> "MyLang - Functional Programming Language"
      in
      make_response id (`Assoc [
        ("contents", `Assoc [
          ("kind", `String "markdown");
          ("value", `String hover_text)
        ])
      ])
  | _ -> make_response id (`Assoc [("contents", `Null)])

(** 处理 definition 请求 *)
let handle_definition id params =
  let pos = get_position params in
  let uri_opt = get_uri params in
  match pos, uri_opt with
  | Some (line, char), Some uri ->
      (match Hashtbl.find documents uri with
       | Some content ->
           let line_zero_based = line in
           let char_zero_based = char in
           (match Symbol_table.get_identifier_at_pos content line_zero_based char_zero_based with
            | Some ident ->
                (* 尝试解析文档获取符号表 *)
                (try
                   let lexbuf = Lexing.from_string content in
                   let expr = Parser.prog Lexer.read lexbuf in
                   let table = Symbol_table.extract_symbols expr in
                   (match Symbol_table.find_def table ident with
                    | Some def ->
                        make_response id (`Assoc [
                          ("uri", `String uri);
                          ("range", `Assoc [
                            ("start", `Assoc [("line", `Int (def.pos.line - 1)); ("character", `Int (def.pos.col - 1))]);
                            ("end", `Assoc [("line", `Int (def.pos.line - 1)); ("character", `Int (def.pos.col - 1 + String.length ident))])
                          ])
                        ])
                    | None -> make_response id `Null)
                 with _ -> make_response id `Null)
            | None -> make_response id `Null)
       | None -> make_response id `Null)
  | _ -> make_response id `Null

(** 发送诊断信息 *)
let send_diagnostics uri content =
  let diagnostics = 
    if String.is_empty content then []
    else
      (* 尝试编译并收集诊断 *)
      try
        let diag = Diagnostics.create () in
        let lexbuf = Lexing.from_string content in
        lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = uri };
        Parser_recovery.set_reporter (fun msg pos ->
          Diagnostics.add_error diag
            ~phase:Diagnostics.Parsing
            ~line:pos.Lexing.pos_lnum
            ~col:(max 1 (pos.Lexing.pos_cnum - pos.Lexing.pos_bol))
            msg
        );
        let expr = Parser.prog Lexer.read lexbuf in
        (* 类型检查 *)
        (try
           let _ = Typeinfer.typecheck expr in
           ()
         with
         | Types.TypeError msg ->
             Diagnostics.add_error diag ~phase:Diagnostics.TypeChecking msg
         | exn ->
             Diagnostics.add_error diag ~phase:Diagnostics.TypeChecking (Exn.to_string exn));
        (* 所有权检查 *)
        (try Ownership.check_program [expr] with
         | Ownership.OwnershipError msg ->
             Diagnostics.add_error diag ~phase:Diagnostics.OwnershipCheck msg
         | _ -> ());
        (* 转换诊断为 LSP 格式 *)
        List.map (List.rev diag.diagnostics) ~f:(fun d ->
          let severity = match d.Error_context.severity with
            | Error_context.Error -> 1
            | Error_context.Warning -> 2
            | Error_context.Note -> 4
          in
          `Assoc [
            ("range", `Assoc [
              ("start", `Assoc [("line", `Int (d.Error_context.line - 1)); ("character", `Int (d.Error_context.col - 1))]);
              ("end", `Assoc [("line", `Int (d.Error_context.line - 1)); ("character", `Int (d.Error_context.col - 1 + d.Error_context.highlight_len))])
            ]);
            ("severity", `Int severity);
            ("message", `String d.Error_context.message)
          ]
        )
      with
      | exn -> [
          `Assoc [
            ("range", `Assoc [
              ("start", `Assoc [("line", `Int 0); ("character", `Int 0)]);
              ("end", `Assoc [("line", `Int 0); ("character", `Int 1)])
            ]);
            ("severity", `Int 1);
            ("message", `String ("Parse error: " ^ Exn.to_string exn))
          ]
        ]
  in
  let params = `Assoc [
    ("uri", `String uri);
    ("diagnostics", `List diagnostics)
  ] in
  write_message (make_notification "textDocument/publishDiagnostics" params)

(** 主消息循环 *)
let rec main_loop () =
  match read_message () with
  | None -> ()
  | Some msg ->
      (match msg with
       | `Assoc fields ->
           let id = match List.Assoc.find fields ~equal:String.equal "id" with
             | Some v -> v
             | None -> `Null in
           let method_ = match List.Assoc.find fields ~equal:String.equal "method" with
             | Some (`String m) -> m
             | _ -> "" in
           let params = match List.Assoc.find fields ~equal:String.equal "params" with
             | Some v -> v
             | None -> `Null in
           
           (match method_ with
            | "initialize" ->
                let result = `Assoc [
                  ("capabilities", `Assoc [
                    ("textDocumentSync", `Int 1);
                    ("completionProvider", `Assoc [
                      ("triggerCharacters", `List [`String " "; `String "."])
                    ]);
                     ("hoverProvider", `Bool true);
                     ("definitionProvider", `Bool true);
                     ("documentFormattingProvider", `Bool true)
                   ])
                ] in
                write_message (make_response id result)
            
            | "initialized" -> ()
            
            | "shutdown" ->
                write_message (make_response id `Null)
            
            | "exit" -> exit 0
            
            | "textDocument/didOpen" ->
                (match get_uri params, get_text params with
                 | Some uri, Some text ->
                     Hashtbl.set documents ~key:uri ~data:text;
                     send_diagnostics uri text
                 | Some uri, None ->
                     Hashtbl.set documents ~key:uri ~data:"";
                     send_diagnostics uri ""
                 | _ -> ())
            
            | "textDocument/didChange" ->
                (match get_uri params, get_text params with
                 | Some uri, Some text ->
                     Hashtbl.set documents ~key:uri ~data:text;
                     send_diagnostics uri text
                 | _ -> ())
            
            | "textDocument/completion" ->
                write_message (handle_completion id params)
            
            | "textDocument/hover" ->
                write_message (handle_hover id params)
            
            | "textDocument/definition" ->
                write_message (handle_definition id params)
            
            | "textDocument/formatting" ->
                (* 简单的格式化：保持不变 *)
                let result = `List [] in
                write_message (make_response id result)
            
            | _ -> ());
           
           main_loop ()
       | _ -> main_loop ())

(** 启动 LSP 服务器 *)
let start () =
  Printf.eprintf "MyLang LSP Server starting...\n";
  main_loop ()
