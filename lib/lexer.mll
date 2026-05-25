{
  open Parser
  exception SyntaxError of string
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha | digit | '_')*
let whitespace = [' ' '\t' '\r']
let newline = '\n'

rule read =
  parse
  | whitespace    { read lexbuf }
  | newline       { read lexbuf }
  | "(*"          { read_comment lexbuf }
  | "true"        { BOOL true }
  | "false"       { BOOL false }
  | "if"          { IF }
  | "then"        { THEN }
  | "else"        { ELSE }
  | "let"         { LET }
  | "rec"         { REC }
  | "in"          { IN }
  | "fun"         { FUN }
  | "->"          { ARROW }
  | "&&"          { AND }
  | "||"          { OR }
  | "not"         { NOT }
  | "::"          { CONS }
  | ";"           { SEMI }
  | "="           { EQ }
  | "<>"          { NEQ }
  | "<"           { LT }
  | "<="          { LE }
  | ">"           { GT }
  | ">="          { GE }
  | "+"           { PLUS }
  | "-"           { MINUS }
  | "*"           { STAR }
  | "/"           { SLASH }
  | "("           { LPAREN }
  | ")"           { RPAREN }
  | "["           { LBRACKET }
  | "]"           { RBRACKET }
  | ","           { COMMA }
  | '"'           { read_string (Buffer.create 256) lexbuf }
  | digit+ as n   { INT (int_of_string n) }
  | ident as s    { IDENT s }
  | _             { raise (SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
  | eof           { EOF }

and read_comment =
  parse
  | "*)"          { read lexbuf }
  | eof           { raise (SyntaxError "Unterminated comment") }
  | _             { read_comment lexbuf }

and read_string buf =
  parse
  | '"'           { STRING (Buffer.contents buf) }
  | '\\' '/'      { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\'     { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'n'      { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'      { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'      { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' '"'      { Buffer.add_char buf '"'; read_string buf lexbuf }
  | [^ '"' '\\']+ as s
                  { Buffer.add_string buf s; read_string buf lexbuf }
  | _             { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof           { raise (SyntaxError "Unterminated string") }
