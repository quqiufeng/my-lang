open Core
open My_lang

let test_count = ref 0
let pass_count = ref 0

let test name code expected =
  incr test_count;
  try
    let result = My_lang.run code in
    let result_str = Ast.string_of_value result in
    if String.equal result_str expected then begin
      incr pass_count;
      Printf.printf "  PASS: %s\n" name
    end else
      Printf.printf "  FAIL: %s (expected %s, got %s)\n" name expected result_str
  with exn ->
    Printf.printf "  FAIL: %s (exception: %s)\n" name (Exn.to_string exn)

let test_error name code =
  incr test_count;
  try
    let _ = My_lang.run code in
    Printf.printf "  FAIL: %s (expected error)\n" name
  with _ ->
    incr pass_count;
    Printf.printf "  PASS: %s (error raised)\n" name

let () =
  Printf.printf "=== New Standard Library Tests ===\n\n";
  
  (* ===== URL 编码测试 ===== *)
  Printf.printf "-- URL Encoding --\n";
  test "url_encode_simple" "url_encode \"hello\"" "\"hello\"";
  test "url_encode_space" "url_encode \"hello world\"" "\"hello%20world\"";
  test "url_encode_special" "url_encode \"a&b=c\"" "\"a%26b%3Dc\"";
  test "url_decode_simple" "url_decode \"hello\"" "\"hello\"";
  test "url_decode_space" "url_decode \"hello%20world\"" "\"hello world\"";
  test "url_decode_plus" "url_decode \"hello+world\"" "\"hello world\"";
  test "url_decode_special" "url_decode \"a%26b%3Dc\"" "\"a&b=c\"";
  
  (* ===== 加密测试 ===== *)
  Printf.printf "\n-- Crypto --\n";
  test "hash_md5" "hash_md5 \"hello\"" "\"5d41402abc4b2a76b9719d911017c592\"";
  test "hash_md5_empty" "hash_md5 \"\"" "\"d41d8cd98f00b204e9800998ecf8427e\"";
  test "hash_sha256" "hash_sha256 \"hello\"" "\"5d41402abc4b2a76b9719d911017c592\"";
  test "base64_encode" "base64_encode \"hello\"" "\"aGVsbG8=\"";
  test "base64_decode" "base64_decode \"aGVsbG8=\"" "\"hello\"";
  test_error "base64_decode_invalid" "base64_decode \"invalid!\"";
  test "hex_encode" "hex_encode \"hello\"" "\"68656C6C6F\"";
  test "hex_decode" "hex_decode \"68656C6C6F\"" "\"hello\"";
  test_error "hex_decode_odd_length" "hex_decode \"123\"";
  
  (* ===== 调试测试 ===== *)
  Printf.printf "\n-- Debug --\n";
  test "debug_type_int" "debug_type 42" "\"int\"";
  test "debug_type_bool" "debug_type true" "\"bool\"";
  test "debug_type_string" "debug_type \"hello\"" "\"string\"";
  test "debug_type_list" "debug_type [1, 2, 3]" "\"list\"";
  test "debug_type_fun" "debug_type (fun x -> x)" "\"function\"";
  test "debug_assert_true" "debug_assert true" "()";
  test_error "debug_assert_false" "debug_assert false";
  test "debug_trace" "debug_trace 42" "42";
  
  (* ===== 并发测试 ===== *)
  Printf.printf "\n-- Concurrency --\n";
  test "mutex_create" "mutex_create ()" "0";
  test "mutex_lock" "let m = mutex_create () in mutex_lock m; m" "0";
  test "mutex_unlock" "let m = mutex_create () in mutex_lock m; mutex_unlock m; m" "0";
  test "channel_create" "channel_create ()" "{buffer = []; closed = false}";
  test "channel_send" "let ch = channel_create () in channel_send (ch, 42); ch" "{buffer = [42]; closed = false}";
  test "channel_receive" "let ch = channel_create () in channel_send (ch, 42); channel_receive ch" "42";
  test_error "channel_receive_empty" "channel_receive (channel_create ())";
  
  (* ===== HTTP 测试 ===== *)
  Printf.printf "\n-- HTTP --\n";
  test "http_get" "debug_type (http_get \"https://httpbin.org/get\")" "\"string\"";
  
  Printf.printf "\n=== Results: %d/%d passed ===\n" !pass_count !test_count
