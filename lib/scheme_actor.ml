(** Actor 并发模型模块

    基于 Chez Scheme 的轻量级线程实现 Erlang 风格的 Actor 模型，
    支持 spawn、send、receive、百万级并发。
*)

open Ast

(** Actor 定义 *)
type actor_def = {
  name : string;                    (* Actor 名 *)
  state_type : string option;       (* 状态类型 *)
  handlers : message_handler list;  (* 消息处理器 *)
}

(** 消息处理器 *)
and message_handler = {
  msg_pattern : string;             (* 消息模式 *)
  msg_params : string list;         (* 参数列表 *)
  msg_body : Ast.expr;              (* 处理体 *)
}

(** 生成 Actor 系统初始化的 Scheme 代码 *)
let compile_actor_system_init () : string =
{|;; Actor 系统初始化
(define actor-mailbox (make-parameter #f))
(define actor-pid-counter (make-parameter 0))

(define (make-actor-pid)
  (let ((id (actor-pid-counter)))
    (actor-pid-counter (+ id 1))
    (string->symbol (string-append "actor-" (number->string id)))))

(define (send pid msg)
  (let ((mailbox (hashtable-ref actor-mailboxes pid #f)))
    (if mailbox
        (mailbox-push! mailbox msg)
        (error 'send "actor not found" pid))))

(define (receive)
  (let ((mailbox (actor-mailbox)))
    (if mailbox
        (mailbox-pop! mailbox)
        (error 'receive "no mailbox"))))

(define (spawn thunk)
  (let* ((pid (make-actor-pid))
         (mailbox (make-mailbox))
         (thread (fork-thread
                   (lambda ()
                     (parameterize ((actor-mailbox mailbox))
                       (thunk))))))
    (hashtable-set! actor-mailboxes pid mailbox)
    pid))

(define actor-mailboxes (make-eq-hashtable))

(define-record-type mailbox
  (fields
    (mutable messages)
    (mutable waiting)))

(define (make-mailbox)
  (make-mailbox '() #f))

(define (mailbox-push! mailbox msg)
  (let ((msgs (mailbox-messages mailbox)))
    (mailbox-messages-set! mailbox (append msgs (list msg)))
    (let ((waiting (mailbox-waiting mailbox)))
      (when waiting
        (mailbox-waiting-set! mailbox #f)
        (waiting msg)))))

(define (mailbox-pop! mailbox)
  (let ((msgs (mailbox-messages mailbox)))
    (if (null? msgs)
        (call/cc (lambda (k)
                   (mailbox-waiting-set! mailbox k)
                   (suspend)))
        (begin
          (mailbox-messages-set! mailbox (cdr msgs))
          (car msgs)))))|}

(** 生成 spawn 操作的 Scheme 代码 *)
let compile_spawn (body : Ast.expr) (compile_expr : Ast.expr -> string) : string =
  Printf.sprintf "(spawn (lambda () %s))" (compile_expr body)

(** 生成 send 操作的 Scheme 代码 *)
let compile_send (pid : Ast.expr) (msg : Ast.expr) (compile_expr : Ast.expr -> string) : string =
  Printf.sprintf "(send %s %s)" (compile_expr pid) (compile_expr msg)

(** 生成 receive 操作的 Scheme 代码 *)
let compile_receive () : string =
  "(receive)"

(** 生成完整的 Actor 程序的 Scheme 代码 *)
let compile_actor_program (expr : Ast.expr) (compile_expr : Ast.expr -> string) : string =
  let actor_init = compile_actor_system_init () in
  let main_code = compile_expr expr in
  Printf.sprintf "(import (chezscheme))\n\n%s\n\n;; 主程序\n%s\n" actor_init main_code
