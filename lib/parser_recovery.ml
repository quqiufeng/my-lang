(** Parser 错误恢复支持模块

    提供解析错误报告机制，避免循环依赖。
*)

type error_reporter = string -> Lexing.position -> unit

let reporter_ref : error_reporter option ref = ref None

let set_reporter f = reporter_ref := Some f

let report_error msg pos =
  match !reporter_ref with
  | Some f -> f msg pos
  | None -> ()

let clear_reporter () = reporter_ref := None
