%{
  open Ast
%}

%token <int> INT
%token <bool> BOOL
%token <string> IDENT
%token <string> STRING
%token LET REC IN FUN ARROW UNDERSCORE
%token IF THEN ELSE WHILE DO DONE MATCH WITH PIPE
%token AND OR NOT
%token EQ NEQ LT LE GT GE
%token PLUS MINUS STAR SLASH
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token COMMA SEMI CONS CARET
%token EOF

%nonassoc IN
%right ARROW
%nonassoc ELSE
%left OR
%left AND
%nonassoc EQ NEQ LT LE GT GE
%right CONS
%left PLUS MINUS CARET
%left STAR SLASH
%nonassoc NOT

%start <Ast.expr> prog

%%

prog:
  | e = expr EOF { e }
  ;

expr:
  | e = simple_expr { e }
  | e = compound_expr { e }
  ;

compound_expr:
  | e1 = expr PLUS e2 = expr   { EAdd (e1, e2) }
  | e1 = expr MINUS e2 = expr  { ESub (e1, e2) }
  | e1 = expr STAR e2 = expr   { EMul (e1, e2) }
  | e1 = expr SLASH e2 = expr  { EDiv (e1, e2) }
  | e1 = expr EQ e2 = expr     { EEq (e1, e2) }
  | e1 = expr NEQ e2 = expr    { ENeq (e1, e2) }
  | e1 = expr LT e2 = expr     { ELt (e1, e2) }
  | e1 = expr LE e2 = expr     { ELe (e1, e2) }
  | e1 = expr GT e2 = expr     { EGt (e1, e2) }
  | e1 = expr GE e2 = expr     { EGe (e1, e2) }
  | e1 = expr AND e2 = expr    { EAnd (e1, e2) }
  | e1 = expr OR e2 = expr     { EOr (e1, e2) }
  | NOT e = expr               { ENot e }
  | e1 = expr CONS e2 = expr   { ECons (e1, e2) }
  | e1 = expr CARET e2 = expr  { ECat (e1, e2) }
  | e1 = expr SEMI e2 = expr   { ESeq (e1, e2) }
  | IF c = expr THEN t = expr ELSE f = expr { EIf (c, t, f) }
  | WHILE c = expr DO body = expr DONE { EWhile (c, body) }
  | LET x = IDENT EQ v = expr IN body = expr { ELet (x, v, body) }
  | LET REC x = IDENT EQ v = expr IN body = expr { ELetRec (x, v, body) }
  | FUN x = IDENT ARROW body = expr { EFun (x, body) }
  | MATCH e = expr WITH cases = match_cases { EMatch (e, cases) }
  | e1 = simple_expr LBRACKET e2 = expr RBRACKET { EIndex (e1, e2) }
  | e1 = simple_expr e2 = simple_expr { EApp (e1, e2) }
  | e1 = compound_expr e2 = simple_expr { EApp (e1, e2) }
  ;

simple_expr:
  | n = INT        { EInt n }
  | b = BOOL       { EBool b }
  | s = STRING     { EString s }
  | x = IDENT      { EVar x }
  | LPAREN RPAREN  { ETuple [] }
  | LPAREN e = expr RPAREN { e }
  | LPAREN e = tuple_elems RPAREN { ETuple e }
  | LBRACKET RBRACKET { EList [] }
  | LBRACKET e = expr RBRACKET { EList [e] }
  | LBRACKET e = expr COMMA es = separated_list(COMMA, expr) RBRACKET { EList (e :: es) }
  ;

tuple_elems:
  | e = expr COMMA es = separated_list(COMMA, expr) { e :: es }
  ;

match_cases:
  | PIPE? cases = separated_list(PIPE, match_case) { cases }
  ;

match_case:
  | p = pattern ARROW e = expr { (p, e) }
  ;

pattern:
  | p = simple_pattern { p }
  | p1 = pattern CONS p2 = pattern { PCons (p1, p2) }
  ;

simple_pattern:
  | UNDERSCORE    { PWildcard }
  | x = IDENT     { if x = "_" then PWildcard else PVar x }
  | n = INT        { PInt n }
  | b = BOOL       { PBool b }
  | s = STRING     { PString s }
  | LPAREN RPAREN  { PUnit }
  | LPAREN p = tuple_pattern RPAREN { PTuple p }
  | LBRACKET RBRACKET { PList [] }
  | LBRACKET p = pattern RBRACKET { PList [p] }
  | LBRACKET p = pattern COMMA ps = separated_list(COMMA, pattern) RBRACKET { PList (p :: ps) }
  ;

tuple_pattern:
  | p = pattern COMMA ps = separated_list(COMMA, pattern) { p :: ps }
  ;
