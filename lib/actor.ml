(** 轻量级 Actor 模型

    基于 OCaml Thread 模块实现 M:N 线程。
    - spawn expr: 创建新 actor，expr 必须是无参函数
    - send pid msg: 向 actor 发送消息
    - receive: 阻塞接收当前 mailbox 中的消息
*)

open Ast

exception ActorError of string

type actor = {
  pid : int;
  mailbox : value Queue.t;
  mailbox_mutex : Mutex.t;
  mailbox_cond : Condition.t;
  mutable active : bool;
}

let actor_table : (int, actor) Hashtbl.t = Hashtbl.create 64
let actor_counter = ref 1
let counter_mutex = Mutex.create ()
let thread_pid_map : (int, int) Hashtbl.t = Hashtbl.create 64

let get_current_pid () =
  let tid = Thread.id (Thread.self ()) in
  match Hashtbl.find_opt thread_pid_map tid with
  | Some pid -> pid
  | None -> 0

let register_thread_pid tid pid =
  Hashtbl.replace thread_pid_map tid pid

(** 主线程注册 *)
let () =
  let main_tid = Thread.id (Thread.self ()) in
  register_thread_pid main_tid 0;
  let main_actor = {
    pid = 0;
    mailbox = Queue.create ();
    mailbox_mutex = Mutex.create ();
    mailbox_cond = Condition.create ();
    active = true;
  } in
  Hashtbl.replace actor_table 0 main_actor

let spawn_actor f =
  Mutex.lock counter_mutex;
  let pid = !actor_counter in
  incr actor_counter;
  Mutex.unlock counter_mutex;
  
  let actor = {
    pid;
    mailbox = Queue.create ();
    mailbox_mutex = Mutex.create ();
    mailbox_cond = Condition.create ();
    active = true;
  } in
  
  Hashtbl.replace actor_table pid actor;
  
  let th = Thread.create (fun () ->
    let tid = Thread.id (Thread.self ()) in
    register_thread_pid tid pid;
    let _ = f () in
    Mutex.lock actor.mailbox_mutex;
    actor.active <- false;
    Condition.broadcast actor.mailbox_cond;
    Mutex.unlock actor.mailbox_mutex
  ) () in
  
  (* 主线程立即注册，避免 race condition *)
  register_thread_pid (Thread.id th) pid;
  
  VInt pid

let send_message pid v =
  match Hashtbl.find_opt actor_table pid with
  | Some actor ->
      Mutex.lock actor.mailbox_mutex;
      Queue.add v actor.mailbox;
      Condition.signal actor.mailbox_cond;
      Mutex.unlock actor.mailbox_mutex
  | None ->
      raise (ActorError ("无效的 actor pid: " ^ string_of_int pid))

let receive_message () =
  let pid = get_current_pid () in
  match Hashtbl.find_opt actor_table pid with
  | Some actor ->
      Mutex.lock actor.mailbox_mutex;
      while Queue.is_empty actor.mailbox && actor.active do
        Condition.wait actor.mailbox_cond actor.mailbox_mutex
      done;
      let result =
        if Queue.is_empty actor.mailbox then VUnit
        else Queue.take actor.mailbox
      in
      Mutex.unlock actor.mailbox_mutex;
      result
  | None ->
      raise (ActorError ("当前线程不是 actor"))
