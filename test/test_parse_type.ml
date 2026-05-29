let () =
  let s = "int list" in
  let st = My_lang.Typeinfer.create_state () in
  let t = My_lang.Typeinfer.parse_type_string st s in
  Printf.printf "'%s' -> %s\n" s (My_lang.Types.string_of_type t)
