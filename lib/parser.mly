%{
  open Ast
%}

%token <int> INT
%token <bool> BOOL
%token <char> CHAR
%token <string> IDENT
%token <string> STRING
%token LET REC IN FUN ARROW UNDERSCORE
%token IF THEN ELSE WHILE DO DONE MATCH WITH PIPE TRY RAISE
%token AND OR NOT TYPE OF REF BANG ASSIGN
%token EQ NEQ LT LE GT GE
%token PLUS MINUS STAR SLASH
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token LARRAY RARRAY
%token LBRACE RBRACE
%token DOTLPAREN
%token DOT
%token COMMA SEMI COLON CONS CARET
%token EOF

%right IN
%right ARROW
%nonassoc ELSE
%left SEMI
%right ASSIGN
%left OR
%left AND
%nonassoc EQ NEQ LT LE GT GE
%right CONS
%left PLUS MINUS CARET
%left STAR SLASH
%nonassoc NOT BANG REF
%nonassoc DOT DOTLPAREN

%start <Ast.expr> prog

%%

prog:
  | e = expr EOF { e }
  ;

expr:
  | e = let_expr { e }
  ;

let_expr:
  | LET x = IDENT EQ v = expr IN body = expr { ELet (x, v, body) }
  | LET REC x = IDENT EQ v = expr IN body = expr { ELetRec (x, v, body) }
  | e = seq_expr { e }
  ;

seq_expr:
  | e1 = if_expr SEMI e2 = seq_expr { ESeq (e1, e2) }
  | e = if_expr { e }
  ;

if_expr:
  | IF c = expr THEN t = expr ELSE f = expr { EIf (c, t, f) }
  | MATCH e = expr WITH cases = match_cases { EMatch (e, cases) }
  | TRY e = expr WITH cases = match_cases { ETry (e, cases) }
  | TYPE x = IDENT EQ ctors = ctor_defs { ETypeDef (x, ctors) }
  | WHILE c = expr DO body = expr DONE { EWhile (c, body) }
  | FUN x = IDENT ARROW body = expr { EFun (x, body) }
  | RAISE e = app_expr { ERaise e }
  | e1 = postfix_expr ASSIGN e2 = if_expr { EAssign (e1, e2) }
  | e = or_expr { e }
  ;

or_expr:
  | e1 = or_expr OR e2 = and_expr { EOr (e1, e2) }
  | e = and_expr { e }
  ;

and_expr:
  | e1 = and_expr AND e2 = comp_expr { EAnd (e1, e2) }
  | e = comp_expr { e }
  ;

comp_expr:
  | e1 = comp_expr EQ e2 = cons_expr { EEq (e1, e2) }
  | e1 = comp_expr NEQ e2 = cons_expr { ENeq (e1, e2) }
  | e1 = comp_expr LT e2 = cons_expr { ELt (e1, e2) }
  | e1 = comp_expr LE e2 = cons_expr { ELe (e1, e2) }
  | e1 = comp_expr GT e2 = cons_expr { EGt (e1, e2) }
  | e1 = comp_expr GE e2 = cons_expr { EGe (e1, e2) }
  | e = cons_expr { e }
  ;

cons_expr:
  | e1 = cat_expr CONS e2 = cons_expr { ECons (e1, e2) }
  | e = cat_expr { e }
  ;

cat_expr:
  | e1 = cat_expr CARET e2 = add_expr { ECat (e1, e2) }
  | e = add_expr { e }
  ;

add_expr:
  | e1 = add_expr PLUS e2 = mul_expr { EAdd (e1, e2) }
  | e1 = add_expr MINUS e2 = mul_expr { ESub (e1, e2) }
  | e = mul_expr { e }
  ;

mul_expr:
  | e1 = mul_expr STAR e2 = app_expr { EMul (e1, e2) }
  | e1 = mul_expr SLASH e2 = app_expr { EDiv (e1, e2) }
  | e = app_expr { e }
  ;

app_expr:
  | e1 = app_expr e2 = unary_expr { EApp (e1, e2) }
  | e = unary_expr { e }
  ;

unary_expr:
  | NOT e = unary_expr { ENot e }
  | BANG e = unary_expr { EDeref e }
  | REF e = unary_expr { ERef e }
  | MINUS e = unary_expr { ESub (EInt 0, e) }
  | e = postfix_expr { e }
  ;

postfix_expr:
  | e = postfix_expr DOTLPAREN idx = expr RPAREN { EArrayGet (e, idx) }
  | e = postfix_expr DOT field = IDENT { ERecordGet (e, field) }
  | e = postfix_expr LBRACKET idx = expr RBRACKET { EIndex (e, idx) }
  | e = postfix_expr LBRACKET start = expr COLON end_ = expr RBRACKET { ESlice (e, Some start, Some end_) }
  | e = primary { e }
  ;

primary:
  | n = INT        { EInt n }
  | b = BOOL       { EBool b }
  | c = CHAR       { EChar c }
  | s = STRING     { EString s }
  | x = IDENT      { if String.length x > 0 && x.[0] >= 'A' && x.[0] <= 'Z' then ECtor (x, None) else EVar x }
  | LPAREN RPAREN  { ETuple [] }
  | LPAREN e = expr RPAREN { e }
  | LPAREN e = tuple_elems RPAREN { ETuple e }
  | LBRACKET RBRACKET { EList [] }
  | LBRACKET e = expr RBRACKET { EList [e] }
  | LBRACKET e = expr COMMA es = separated_list(COMMA, expr) RBRACKET { EList (e :: es) }
  | LARRAY RARRAY { EArray [] }
  | LARRAY e = expr COMMA es = separated_list(COMMA, expr) RARRAY { EArray (e :: es) }
  | LBRACE RBRACE { ERecord [] }
  | LBRACE fields = record_fields RBRACE { ERecord fields }
  ;

record_fields:
  | f = record_field { [f] }
  | f = record_field SEMI fs = record_fields { f :: fs }
  ;

record_field:
  | name = IDENT EQ value = if_expr { (name, value) }
  ;

tuple_elems:
  | e = expr COMMA es = separated_list(COMMA, expr) { e :: es }
  ;

ctor_defs:
  | PIPE? defs = separated_list(PIPE, ctor_def) { defs }
  ;

ctor_def:
  | c = IDENT { (c, None) }
  | c = IDENT OF t = IDENT { (c, Some t) }
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
  | x = IDENT     { if x = "_" then PWildcard else if String.length x > 0 && x.[0] >= 'A' && x.[0] <= 'Z' then PCtor (x, None) else PVar x }
  | c = IDENT p = simple_pattern
    { if String.length c > 0 && c.[0] >= 'A' && c.[0] <= 'Z' then PCtor (c, Some p)
      else PVar c }
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