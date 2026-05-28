{
open Parser
exception LexError of string
}

rule token = parse
  | [' ' '\t' '\n']     { token lexbuf }
  | "//" [^ '\n']* '\n' { token lexbuf }
  | "/*"                { comment 0 lexbuf }
  | ['0'-'9']+          { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | ['0'-'9']+ '.' ['0'-'9']+ { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "true"              { TRUE }
  | "false"             { FALSE }
  | "if"                { IF }
  | "else"              { ELSE }
  | "while"             { WHILE }
  | "for"               { FOR }
  | "return"            { RETURN }
  | "fun"               { FUN }
  | "let"               { LET }
  | "in"                { IN }
  | "type"              { TYPE }
  | "match"             { MATCH }
  | "with"              { WITH }
  | "of"                { OF }
  | "module"            { MODULE }
  | "sig"               { SIG }
  | "struct"            { STRUCT }
  | "end"               { END }
  | "import"            { IMPORT }
  | "export"            { EXPORT }
  | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
                        { IDENT (Lexing.lexeme lexbuf) }
  | '"'                 { string_literal (Buffer.create 16) lexbuf }
  | "'" [^ '\\' '\''] "'" { CHAR (Lexing.lexeme lexbuf).[1] }
  | "'" '\\' ['\\' '\'' 'n' 't' 'r' '0'] "'" 
                        { let s = Lexing.lexeme lexbuf in
                          match s.[2] with
                          | '\\' -> CHAR '\\'
                          | '\'' -> CHAR '\''
                          | 'n' -> CHAR '\n'
                          | 't' -> CHAR '\t'
                          | 'r' -> CHAR '\r'
                          | '0' -> CHAR '\000'
                          | _ -> raise (LexError ("Invalid escape: " ^ s)) }
  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | '*'                 { STAR }
  | '/'                 { SLASH }
  | '%'                 { PERCENT }
  | "=="                { EQ }
  | "!="                { NEQ }
  | '<'                 { LT }
  | '>'                 { GT }
  | "<="                { LE }
  | ">="                { GE }
  | "&&"                { AND }
  | "||"                { OR }
  | '!'                 { NOT }
  | '='                 { ASSIGN }
  | "+="                { PLUS_ASSIGN }
  | "-="                { MINUS_ASSIGN }
  | "*="                { STAR_ASSIGN }
  | "/="                { SLASH_ASSIGN }
  | "->"                { ARROW }
  | "=>"                { FAT_ARROW }
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | '{'                 { LBRACE }
  | '}'                 { RBRACE }
  | '['                 { LBRACKET }
  | ']'                 { RBRACKET }
  | ','                 { COMMA }
  | ';'                 { SEMICOLON }
  | ':'                 { COLON }
  | '.'                 { DOT }
  | '|'                 { PIPE }
  | '&'                 { AMPERSAND }
  | '^'                 { CARET }
  | '~'                 { TILDE }
  | "<<"                { LSHIFT }
  | ">>"                { RSHIFT }
  | '#'                 { HASH }
  | '@'                 { AT }
  | '_'                 { UNDERSCORE }
  | eof                 { EOF }
  | _                   { raise (LexError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and comment depth = parse
  | "*/"                { if depth = 0 then token lexbuf 
                          else comment (depth - 1) lexbuf }
  | "/*"                { comment (depth + 1) lexbuf }
  | _                   { comment depth lexbuf }
  | eof                 { raise (LexError "Unterminated comment") }

and string_literal buf = parse
  | '"'                 { STRING (Buffer.contents buf) }
  | '\\' ['\\' '"' 'n' 't' 'r' '0']
                        { let c = match Lexing.lexeme lexbuf with
                          | "\\n" -> '\n'
                          | "\\t" -> '\t'
                          | "\\r" -> '\r'
                          | "\\0" -> '\000'
                          | "\\\\" -> '\\'
                          | "\\\"" -> '"'
                          | _ -> raise (LexError "Invalid escape") in
                          Buffer.add_char buf c;
                          string_literal buf lexbuf }
  | [^ '\\' '"']+       { Buffer.add_string buf (Lexing.lexeme lexbuf);
                          string_literal buf lexbuf }
  | eof                 { raise (LexError "Unterminated string") }
  | _                   { raise (LexError ("Invalid string char: " ^ Lexing.lexeme lexbuf)) }
