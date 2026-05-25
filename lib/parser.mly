%{
  open Ast
%}

%token <int> INT
%token <bool> BOOL
%token <string> IDENT
%token <string> STRING
%token LET REC IN FUN ARROW
%token IF THEN ELSE
%token AND OR NOT
%token EQ NEQ LT LE GT GE
%token PLUS MINUS STAR SLASH
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token COMMA SEMI CONS
%token EOF

%nonassoc IN
%right ARROW
%nonassoc ELSE
%left OR
%left AND
%nonassoc EQ NEQ LT LE GT GE
%right CONS
%left PLUS MINUS
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
  | e1 = expr SEMI e2 = expr   { ESeq (e1, e2) }
  | IF c = expr THEN t = expr ELSE f = expr { EIf (c, t, f) }
  | LET x = IDENT EQ v = expr IN body = expr { ELet (x, v, body) }
  | LET REC x = IDENT EQ v = expr IN body = expr { ELetRec (x, v, body) }
  | FUN x = IDENT ARROW body = expr { EFun (x, body) }
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
  | LBRACKET es = comma_list RBRACKET { EList es }
  ;

tuple_elems:
  | e = expr COMMA es = separated_list(COMMA, expr) { e :: es }
  ;

comma_list:
  | e = expr COMMA es = separated_list(COMMA, expr) { e :: es }
  ;
