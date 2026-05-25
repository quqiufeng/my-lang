{
  open Parser
  exception SyntaxError of string
  
  type pos = { line : int; col : int }
  
  let curr_pos = ref { line = 1; col = 1 }
  
  let advance_line () =
    curr_pos := { line = !curr_pos.line + 1; col = 1 }
  
  let advance_col n =
    curr_pos := { line = !curr_pos.line; col = !curr_pos.col + n }
  
  let pos_string () =
    Printf.sprintf "line %d, column %d" !curr_pos.line !curr_pos.col
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha | digit | '_')*
let whitespace = [' ' '\t' '\r']
let newline = '\n'

rule read =
  parse
  | ""            { curr_pos := { line = 1; col = 1 }; read_real lexbuf }

and read_real =
  parse
  | whitespace    { advance_col 1; read_real lexbuf }
  | newline       { advance_line (); read_real lexbuf }
  | "(*"          { advance_col 2; read_comment lexbuf }
  | "true"        { advance_col 4; BOOL true }
  | "false"       { advance_col 5; BOOL false }
  | "if"          { advance_col 2; IF }
  | "then"        { advance_col 4; THEN }
  | "else"        { advance_col 4; ELSE }
  | "while"       { advance_col 5; WHILE }
  | "do"          { advance_col 2; DO }
  | "done"        { advance_col 4; DONE }
  | "match"       { advance_col 5; MATCH }
  | "with"        { advance_col 4; WITH }
  | "let"         { advance_col 3; LET }
  | "rec"         { advance_col 3; REC }
  | "in"          { advance_col 2; IN }
  | "fun"         { advance_col 3; FUN }
  | "->"          { advance_col 2; ARROW }
  | "_"           { advance_col 1; UNDERSCORE }
  | "|"           { advance_col 1; PIPE }
  | "&&"          { advance_col 2; AND }
  | "||"          { advance_col 2; OR }
  | "not"         { advance_col 3; NOT }
  | "::"          { advance_col 2; CONS }
  | "^"           { advance_col 1; CARET }
  | ";"           { advance_col 1; SEMI }
  | "="           { advance_col 1; EQ }
  | "<>"          { advance_col 2; NEQ }
  | "<"           { advance_col 1; LT }
  | "<="          { advance_col 2; LE }
  | ">"           { advance_col 1; GT }
  | ">="          { advance_col 2; GE }
  | "+"           { advance_col 1; PLUS }
  | "-"           { advance_col 1; MINUS }
  | "*"           { advance_col 1; STAR }
  | "/"           { advance_col 1; SLASH }
  | "("           { advance_col 1; LPAREN }
  | ")"           { advance_col 1; RPAREN }
  | "["           { advance_col 1; LBRACKET }
  | "]"           { advance_col 1; RBRACKET }
  | ","           { advance_col 1; COMMA }
  | '"'           { advance_col 1; read_string (Buffer.create 256) lexbuf }
  | digit+ as n   { advance_col (String.length n); INT (int_of_string n) }
  | ident as s    { advance_col (String.length s); IDENT s }
  | _             { raise (SyntaxError ("Unexpected character at " ^ pos_string () ^ ": " ^ Lexing.lexeme lexbuf)) }
  | eof           { EOF }

and read_comment =
  parse
  | "*)"          { advance_col 2; read_real lexbuf }
  | newline       { advance_line (); read_comment lexbuf }
  | eof           { raise (SyntaxError ("Unterminated comment at " ^ pos_string ())) }
  | _             { advance_col 1; read_comment lexbuf }

and read_string buf =
  parse
  | '"'           { advance_col 1; STRING (Buffer.contents buf) }
  | '\\' '/'      { advance_col 2; Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\'     { advance_col 2; Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'n'      { advance_col 2; Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'      { advance_col 2; Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'      { advance_col 2; Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' '"'      { advance_col 2; Buffer.add_char buf '"'; read_string buf lexbuf }
  | [^ '"' '\\']+ as s
                  { advance_col (String.length s); Buffer.add_string buf s; read_string buf lexbuf }
  | _             { raise (SyntaxError ("Illegal string character at " ^ pos_string () ^ ": " ^ Lexing.lexeme lexbuf)) }
  | eof           { raise (SyntaxError ("Unterminated string at " ^ pos_string ())) }
