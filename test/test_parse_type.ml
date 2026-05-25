let () =
  let s = "int list" in
  let t = My_lang.Typeinfer.parse_type_string s in
  Printf.printf "'%s' -> %s\n" s (My_lang.Types.string_of_type t)
