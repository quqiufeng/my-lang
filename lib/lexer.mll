{
  open Parser
  exception SyntaxError of string
  
  let pos_string lexbuf =
    let pos = Lexing.lexeme_start_p lexbuf in
    Printf.sprintf "line %d, column %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = (alpha | '_') (alpha | digit | '_')*
let type_var = '\'' alpha (alpha | digit | '_')*
let whitespace = [' ' '\t' '\r']
let newline = '\n'

rule read =
  parse
  | ""            { read_real lexbuf }

and read_real =
  parse
  | whitespace    { read_real lexbuf }
  | newline       { read_real lexbuf }
  | "(*"          { read_comment lexbuf }
  | "true"        { BOOL true }
  | "false"       { BOOL false }
  | "if"          { IF }
  | "then"        { THEN }
  | "else"        { ELSE }
  | "while"       { WHILE }
  | "do"          { DO }
  | "done"        { DONE }
  | "match"       { MATCH }
  | "with"        { WITH }
  | "try"         { TRY }
  | "raise"       { RAISE }
  | "let"         { LET }
  | "rec"         { REC }
  | "in"          { IN }
  | "type"        { TYPE }
  | "of"          { OF }
  | "with"        { WITH }
  | "fun"         { FUN }
  | "assert"      { ASSERT }
  | "ignore"      { IGNORE }
  | "todo"        { TODO }
  | "ref"         { REF }
  | "module"      { MODULE }
  | "open"        { OPEN }
  | "struct"      { STRUCT }
  | "sig"         { SIG }
  | "end"         { END }
  | "trait"       { TRAIT }
  | "impl"        { IMPL }
  | "for"         { FOR }
  | "self"        { SELF }
  | "spawn"       { SPAWN }
  | "send"        { SEND }
  | "receive"     { RECEIVE }
  | "effect"      { EFFECT }
  | "perform"     { PERFORM }
  | "handle"      { HANDLE }
  | "->"          { ARROW }
  | "!"           { BANG }
  | ":="          { ASSIGN }
  | "<-"          { ASSIGN }
  | "_"           { UNDERSCORE }
  | "|>"          { PIPE_GT }
  | "|"           { PIPE }
  | "&&"          { AND }
  | "||"          { OR }
  | "not"         { NOT }
  | "::"          { CONS }
  | "^"           { CARET }
  | ";"           { SEMI }
  | ":"           { COLON }
  | ".."          { DOTDOT }
  | "="           { EQ }
  | "<>"          { NEQ }
  | "<"           { LT }
  | "<="          { LE }
  | ">"           { GT }
  | ">="          { GE }
  | "+"           { PLUS }
  | '-' digit+ as n { INT (int_of_string n) }
  | "-"           { MINUS }
  | "*"           { STAR }
  | "/"           { SLASH }
  | "("           { LPAREN }
  | ")"           { RPAREN }
  | "{"           { LBRACE }
  | "}"           { RBRACE }
  | "["           { LBRACKET }
  | "]"           { RBRACKET }
  | "[|"          { LARRAY }
  | "|]"          { RARRAY }
  | ".("          { DOTLPAREN }
  | "."           { DOT }
  | ","           { COMMA }
  | '"'           { read_string (Buffer.create 256) lexbuf }
  | "\\n"         { CHAR '\n' }
  | "\\r"         { CHAR '\r' }
  | "\\t"         { CHAR '\t' }
  | "\\\\"        { CHAR '\\' }
  | "\\'"         { CHAR '\'' }
  | '\'' alpha '\'' { let c = Lexing.lexeme_char lexbuf 1 in CHAR c }
  | type_var as s { TYPE_VAR s }
  | digit+ as n   { INT (int_of_string n) }
  | ident as s    { IDENT s }
  | _             { raise (SyntaxError ("Unexpected character at " ^ pos_string lexbuf ^ ": " ^ Lexing.lexeme lexbuf)) }
  | eof           { EOF }

and read_comment =
  parse
  | "*)"          { read_real lexbuf }
  | newline       { read_comment lexbuf }
  | eof           { raise (SyntaxError ("Unterminated comment at " ^ pos_string lexbuf)) }
  | _             { read_comment lexbuf }

and read_char =
  parse
  | '\\' 'n' '\'' { CHAR '\n' }
  | '\\' 'r' '\'' { CHAR '\r' }
  | '\\' 't' '\'' { CHAR '\t' }
  | '\\' '\\' '\'' { CHAR '\\' }
  | '\\' '\'' '\'' { CHAR '\'' }
  | _ '\''         { let c = Lexing.lexeme_char lexbuf 0 in CHAR c }
  | _              { raise (SyntaxError ("Illegal character literal at " ^ pos_string lexbuf)) }
  | eof            { raise (SyntaxError ("Unterminated character literal at " ^ pos_string lexbuf)) }

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
  | _             { raise (SyntaxError ("Illegal string character at " ^ pos_string lexbuf ^ ": " ^ Lexing.lexeme lexbuf)) }
  | eof           { raise (SyntaxError ("Unterminated string at " ^ pos_string lexbuf)) }
