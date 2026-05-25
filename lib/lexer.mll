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
  | "in"          { IN }
  | "fun"         { FUN }
  | "->"          { ARROW }
  | "&&"          { AND }
  | "||"          { OR }
  | "not"         { NOT }
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
  | digit+ as n   { INT (int_of_string n) }
  | ident as s    { IDENT s }
  | _             { raise (SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }
  | eof           { EOF }

and read_comment =
  parse
  | "*)"          { read lexbuf }
  | eof           { raise (SyntaxError "Unterminated comment") }
  | _             { read_comment lexbuf }
