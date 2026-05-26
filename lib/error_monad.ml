(** 统一错误单子

    提供标准的 Result 类型和错误处理组合子，
    统一整个编译器中的错误处理风格。
*)

open Core

type 'a t = ('a, string) Result.t

let ok x = Ok x
let error msg = Error msg

let bind f = function
  | Ok x -> f x
  | Error msg -> Error msg

let map f = function
  | Ok x -> Ok (f x)
  | Error msg -> Error msg

let bind_result r f = bind f r
let map_result r f = map f r

let catch f ~on_error =
  try f ()
  with exn -> Error (on_error exn)

let catch_std f =
  try Ok (f ())
  with exn -> Error (Exn.to_string exn)

let map_error f = function
  | Ok x -> Ok x
  | Error msg -> Error (f msg)

let with_context ctx = map_error (fun msg -> ctx ^ ": " ^ msg)

let sequence results =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | Ok x :: xs -> loop (x :: acc) xs
    | Error msg :: _ -> Error msg
  in
  loop [] results

let mapM f lst =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | x :: xs ->
        match f x with
        | Ok y -> loop (y :: acc) xs
        | Error msg -> Error msg
  in
  loop [] lst

let foldM f init lst =
  let rec loop acc = function
    | [] -> Ok acc
    | x :: xs ->
        match f acc x with
        | Ok acc' -> loop acc' xs
        | Error msg -> Error msg
  in
  loop init lst

let to_option = function
  | Ok x -> Some x
  | Error _ -> None

let of_option ~error = function
  | Some x -> Ok x
  | None -> Error error

let is_ok = function
  | Ok _ -> true
  | Error _ -> false

let is_error = function
  | Ok _ -> false
  | Error _ -> true

let get_ok = function
  | Ok x -> x
  | Error msg -> failwith ("Expected Ok but got Error: " ^ msg)

let get_error = function
  | Ok _ -> failwith "Expected Error but got Ok"
  | Error msg -> msg
