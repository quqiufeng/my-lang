%{
  open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token <char> CHAR
%token <string> IDENT
%token TRUE FALSE
%token IF ELSE WHILE FOR RETURN
%token LET IN FUN
%token TYPE MATCH WITH OF
%token MODULE SIG STRUCT END
%token IMPORT EXPORT
%token PLUS MINUS STAR SLASH PERCENT
%token EQ NEQ LT GT LE GE
%token AND OR NOT
%token ASSIGN PLUS_ASSIGN MINUS_ASSIGN STAR_ASSIGN SLASH_ASSIGN
%token ARROW FAT_ARROW
%token LPAREN RPAREN LBRACE RBRACKET RBRACKET
%token COMMA SEMICOLON COLON DOT
%token PIPE AMPERSAND CARET TILDE LSHIFT RSHIFT
%token HASH AT UNDERSCORE
%token EOF

%left OR
%left AND
%left EQ NEQ
%left LT GT LE GE
%left PLUS MINUS
%left STAR SLASH PERCENT
%left LSHIFT RSHIFT
%left AMPERSAND
%left CARET
%left PIPE
%nonassoc NOT UNARY_MINUS

%start <Ast.program> program

%%

program:
  | stmts EOF { $1 }
;

stmts:
  | /* empty */ { [] }
  | stmt stmts { $1 :: $2 }
;

stmt:
  | LET IDENT ASSIGN expr SEMICOLON { Let ($2, $4) }
  | LET IDENT COLON typ ASSIGN expr SEMICOLON { LetTyped ($2, $4, $6) }
  | FUN IDENT LPAREN params RPAREN COLON typ LBRACE stmts RBRACE
    { FunDef ($2, $4, $7, $9) }
  | TYPE IDENT ASSIGN typ SEMICOLON { TypeAlias ($2, $4) }
  | TYPE IDENT EQ variants SEMICOLON { Variant ($2, $4) }
  | TYPE IDENT COLON COLON EQ LBRACE fields RBRACE SEMICOLON { Record ($2, $7) }
  | MODULE IDENT ASSIGN STRUCT stmts END SEMICOLON { Module ($2, $5) }
  | MODULE IDENT COLON COLON EQ SIG stmts END SEMICOLON { ModuleSig ($2, $7) }
  | IMPORT IDENT SEMICOLON { Import $2 }
  | EXPORT IDENT SEMICOLON { Export $2 }
  | expr SEMICOLON { ExprStmt $1 }
;

params:
  | /* empty */ { [] }
  | param { [$1] }
  | param COMMA params { $1 :: $3 }
;

param:
  | IDENT COLON typ { ($1, $3) }
;

typ:
  | IDENT { TName $1 }
  | typ ARROW typ { TFun ($1, $3) }
  | IDENT LT typs GT { TGeneric ($1, $3) }
  | LPAREN typ RPAREN { $2 }
;

typs:
  | typ { [$1] }
  | typ COMMA typs { $1 :: $3 }
;

variants:
  | variant { [$1] }
  | variant PIPE variants { $1 :: $3 }
;

variant:
  | IDENT { ($1, None) }
  | IDENT OF typ { ($1, Some $3) }
;

fields:
  | field { [$1] }
  | field COMMA fields { $1 :: $3 }
;

field:
  | IDENT COLON typ { ($1, $3) }
;

expr:
  | INT { Int $1 }
  | FLOAT { Float $1 }
  | STRING { String $1 }
  | CHAR { Char $1 }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | IDENT { Var $1 }
  | expr PLUS expr { BinOp (Add, $1, $3) }
  | expr MINUS expr { BinOp (Sub, $1, $3) }
  | expr STAR expr { BinOp (Mul, $1, $3) }
  | expr SLASH expr { BinOp (Div, $1, $3) }
  | expr PERCENT expr { BinOp (Mod, $1, $3) }
  | expr EQ expr { BinOp (Eq, $1, $3) }
  | expr NEQ expr { BinOp (Neq, $1, $3) }
  | expr LT expr { BinOp (Lt, $1, $3) }
  | expr GT expr { BinOp (Gt, $1, $3) }
  | expr LE expr { BinOp (Le, $1, $3) }
  | expr GE expr { BinOp (Ge, $1, $3) }
  | expr AND expr { BinOp (And, $1, $3) }
  | expr OR expr { BinOp (Or, $1, $3) }
  | expr LSHIFT expr { BinOp (LShift, $1, $3) }
  | expr RSHIFT expr { BinOp (RShift, $1, $3) }
  | expr AMPERSAND expr { BinOp (BitAnd, $1, $3) }
  | expr CARET expr { BinOp (BitXor, $1, $3) }
  | expr PIPE expr { BinOp (BitOr, $1, $3) }
  | NOT expr %prec NOT { UnaryOp (Not, $2) }
  | MINUS expr %prec UNARY_MINUS { UnaryOp (Neg, $2) }
  | TILDE expr { UnaryOp (BitNot, $2) }
  | expr LPAREN args RPAREN { Call ($1, $3) }
  | expr DOT IDENT { Field ($1, $3) }
  | expr LBRACKET expr RBRACKET { Index ($1, $3) }
  | LBRACKET exprs RBRACKET { Array $2 }
  | LBRACE fields_exprs RBRACE { RecordLit $2 }
  | LPAREN expr RPAREN { $2 }
  | LPAREN expr COMMA exprs RPAREN { Tuple ($2 :: $4) }
  | FUN LPAREN params RPAREN ARROW expr { Lambda ($3, $6) }
  | IF expr LBRACE stmts RBRACE { If ($2, $4, None) }
  | IF expr LBRACE stmts RBRACE ELSE LBRACE stmts RBRACE { If ($2, $4, Some $8) }
  | WHILE expr LBRACE stmts RBRACE { While ($2, $4) }
  | FOR LPAREN stmt expr SEMICOLON stmt RPAREN LBRACE stmts RBRACE { For ($3, $4, $6, $9) }
  | MATCH expr WITH matches { Match ($2, $4) }
  | LET IDENT ASSIGN expr IN expr { LetIn ($2, $4, $6) }
  | LET IDENT COLON typ ASSIGN expr IN expr { LetInTyped ($2, $4, $6, $8) }
  | expr COLON COLON IDENT { ModuleAccess ($1, $4) }
;

args:
  | /* empty */ { [] }
  | expr { [$1] }
  | expr COMMA args { $1 :: $3 }
;

exprs:
  | /* empty */ { [] }
  | expr { [$1] }
  | expr COMMA exprs { $1 :: $3 }
;

fields_exprs:
  | /* empty */ { [] }
  | field_expr { [$1] }
  | field_expr COMMA fields_exprs { $1 :: $3 }
;

field_expr:
  | IDENT COLON expr { ($1, $3) }
  | IDENT { ($1, Var $1) }
;

matches:
  | /* empty */ { [] }
  | match_case { [$1] }
  | match_case PIPE matches { $1 :: $3 }
;

match_case:
  | pattern ARROW expr { ($1, $3) }
;

pattern:
  | INT { PInt $1 }
  | FLOAT { PFloat $1 }
  | STRING { PString $1 }
  | CHAR { PChar $1 }
  | TRUE { PBool true }
  | FALSE { PBool false }
  | IDENT { PVar $1 }
  | UNDERSCORE { PWildcard }
  | IDENT LPAREN patterns RPAREN { PVariant ($1, $3) }
  | LPAREN pattern RPAREN { $2 }
  | LPAREN pattern COMMA patterns RPAREN { PTuple ($2 :: $4) }
  | pattern PIPE pattern { POr ($1, $3) }
  | pattern WHEN expr { PWhen ($1, $3) }
;

patterns:
  | /* empty */ { [] }
  | pattern { [$1] }
  | pattern COMMA patterns { $1 :: $3 }
;
