open Core
open My_lang

let test_resource_manager_cleanup () =
  let file_created = ref false in
  try
    Resource_manager.with_temp_file "test" ".txt" (fun path ->
      file_created := true;
      Out_channel.write_all path ~data:"test";
      if Stdlib.Sys.file_exists path then
        printf "[PASS] test_resource_manager_cleanup: temp file created\n"
      else
        printf "[FAIL] test_resource_manager_cleanup: temp file not created\n"
    );
    (* 文件应该已被清理 *)
    printf "[INFO] test_resource_manager_cleanup completed\n"
  with exn ->
    printf "[FAIL] test_resource_manager_cleanup: exception %s\n" (Exn.to_string exn)

let test_list_nth_safe () =
  let lst = [1; 2; 3; 4; 5] in
  match Eval.list_nth_safe lst 2 with
  | Some 3 -> printf "[PASS] test_list_nth_safe: correct value at index 2\n"
  | Some n -> printf "[FAIL] test_list_nth_safe: expected 3, got %d\n" n
  | None -> printf "[FAIL] test_list_nth_safe: expected Some 3\n"

let test_list_nth_safe_out_of_bounds () =
  let lst = [1; 2; 3] in
  match Eval.list_nth_safe lst 10 with
  | None -> printf "[PASS] test_list_nth_safe_out_of_bounds: correctly returned None\n"
  | Some n -> printf "[FAIL] test_list_nth_safe_out_of_bounds: expected None, got %d\n" n

let test_list_nth_safe_negative () =
  let lst = [1; 2; 3] in
  match Eval.list_nth_safe lst (-1) with
  | None -> printf "[PASS] test_list_nth_safe_negative: correctly returned None\n"
  | Some n -> printf "[FAIL] test_list_nth_safe_negative: expected None, got %d\n" n

let () =
  test_resource_manager_cleanup ();
  test_list_nth_safe ();
  test_list_nth_safe_out_of_bounds ();
  test_list_nth_safe_negative ();
  printf "\nRobustness tests completed.\n"
