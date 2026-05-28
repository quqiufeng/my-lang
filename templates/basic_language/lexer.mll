(* 基础语言模板 - 词法分析器 *)

{
open Parser

exception SyntaxError of string
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (alpha | digit | '_')*
let integer = '-'? digit+
let whitespace = [' ' '\t' '\n' '\r']+

rule read = parse
  | whitespace { read lexbuf }
  | "(*" { comment 0 lexbuf }
  | integer { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "true" { TRUE }
  | "false" { FALSE }
  | "let" { LET }
  | "in" { IN }
  | "fun" { FUN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "match" { MATCH }
  | "with" { WITH }
  | "()" { UNIT }
  | "->" { ARROW }
  | "|" { PIPE }
  | "=" { EQ }
  | "<>" { NEQ }
  | "<" { LT }
  | "<=" { LE }
  | ">" { GT }
  | ">=" { GE }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "/" { SLASH }
  | "&&" { AND }
  | "||" { OR }
  | "not" { NOT }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "::" { CONS }
  | "," { COMMA }
  | ";" { SEMI }
  | ident { IDENT (Lexing.lexeme lexbuf) }
  | '"' { string (Buffer.create 16) lexbuf }
  | eof { EOF }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and string buf = parse
  | '"' { STRING (Buffer.contents buf) }
  | '\\' '"' { Buffer.add_char buf '"'; string buf lexbuf }
  | '\\' 'n' { Buffer.add_char buf '\n'; string buf lexbuf }
  | '\\' 't' { Buffer.add_char buf '\t'; string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; string buf lexbuf }
  | eof { raise (SyntaxError "Unterminated string") }
  | _ { Buffer.add_char buf (Lexing.lexeme_char lexbuf 0); string buf lexbuf }

and comment depth = parse
  | "*)" { if depth = 0 then read lexbuf else comment (depth - 1) lexbuf }
  | "(*" { comment (depth + 1) lexbuf }
  | eof { raise (SyntaxError "Unterminated comment") }
  | _ { comment depth lexbuf }
