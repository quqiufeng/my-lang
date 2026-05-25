(** 通用 LSP 语言服务器框架

    基于 Language 接口，提供基础的 LSP 功能。
    新语言可以通过配置补全项和 hover 提示来定制。
*)

open Core
open Language_intf

module Make (L : Language) = struct
  type json = Yojson.Safe.t
  
  let read_message () =
    let rec read_headers acc =
      match In_channel.input_line In_channel.stdin with
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
                if n = 0 then () else read_exactly (offset + n)
            in
            read_exactly 0;
            Some (Yojson.Safe.from_string (Bytes.to_string buf))
        | None -> None
  
  let write_message json =
    let content = Yojson.Safe.to_string json in
    let len = String.length content in
    Out_channel.printf "Content-Length: %d\r\n\r\n%s" len content;
    Out_channel.flush stdout
  
  let make_response id result =
    `Assoc [("jsonrpc", `String "2.0"); ("id", id); ("result", result)]
  
  let make_notification method_ params =
    `Assoc [("jsonrpc", `String "2.0"); ("method", `String method_); ("params", params)]
  
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
  
  let documents : (string, string) Hashtbl.t = Hashtbl.create (module String)
  
  let send_diagnostics uri =
    let diagnostics = [] in
    let params = `Assoc [("uri", `String uri); ("diagnostics", `List diagnostics)] in
    write_message (make_notification "textDocument/publishDiagnostics" params)
  
  let rec main_loop () =
    match read_message () with
    | None -> ()
    | Some msg ->
        (match msg with
         | `Assoc fields ->
             let id = match List.Assoc.find fields ~equal:String.equal "id" with Some v -> v | None -> `Null in
             let method_ = match List.Assoc.find fields ~equal:String.equal "method" with
               | Some (`String m) -> m | _ -> "" in
             let params = match List.Assoc.find fields ~equal:String.equal "params" with
               | Some v -> v | None -> `Null in
             
             (match method_ with
              | "initialize" ->
                  let result = `Assoc [
                    ("capabilities", `Assoc [
                      ("textDocumentSync", `Int 1);
                      ("completionProvider", `Assoc [("triggerCharacters", `List [`String " "; `String "."])]);
                      ("hoverProvider", `Bool true);
                      ("documentFormattingProvider", `Bool true)
                    ])
                  ] in
                  write_message (make_response id result)
              | "initialized" -> ()
              | "shutdown" -> write_message (make_response id `Null)
              | "exit" -> exit 0
              | "textDocument/didOpen" | "textDocument/didChange" ->
                  (match get_uri params, get_text params with
                   | Some uri, Some text -> Hashtbl.set documents ~key:uri ~data:text; send_diagnostics uri
                   | Some uri, None -> Hashtbl.set documents ~key:uri ~data:""; send_diagnostics uri
                   | _ -> ())
              | "textDocument/completion" -> write_message (make_response id (`List []))
              | "textDocument/hover" -> write_message (make_response id (`Assoc [("contents", `Null)]))
              | "textDocument/formatting" -> write_message (make_response id (`List []))
              | _ -> ());
             main_loop ()
         | _ -> main_loop ())
  
  let start () =
    Printf.eprintf "%s LSP Server starting...\n" L.name;
    main_loop ()
end
