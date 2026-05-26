%{
  open Ast
%}

%token <int> INT
%token <bool> BOOL
%token <char> CHAR
%token <string> IDENT
%token <string> STRING
%token LET REC IN FUN ARROW UNDERSCORE
%token IF THEN ELSE WHILE DO DONE MATCH WITH PIPE TRY RAISE ASSERT IGNORE
%token AND OR NOT TYPE OF REF BANG ASSIGN PIPE_GT TODO
%token MODULE OPEN STRUCT SIG END TRAIT IMPL FOR SELF
%token <string> TYPE_VAR
%token EQ NEQ LT LE GT GE
%token PLUS MINUS STAR SLASH
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token LARRAY RARRAY
%token LBRACE RBRACE
%token DOTLPAREN
%token DOT
%token COMMA SEMI COLON CONS CARET DOTDOT
%token EOF

%right IN
%right ARROW
%nonassoc ELSE
%left SEMI
%left PIPE_GT
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
  | e = seq_expr { e }
  ;

seq_expr:
  | e1 = let_expr SEMI e2 = seq_expr { ESeq (e1, e2) }
  | e = let_expr { e }
  ;

let_expr:
  | LET x = IDENT COLON t = IDENT EQ v = expr IN body = expr { ELet (x, EAnnot (v, t), body) }
  | LET x = IDENT EQ v = expr IN body = expr { ELet (x, v, body) }
  | LET REC x = IDENT COLON t = IDENT EQ v = expr IN body = expr { ELetRec (x, EAnnot (v, t), body) }
  | LET REC x = IDENT EQ v = expr IN body = expr { ELetRec (x, v, body) }
  | MODULE x = IDENT EQ body = module_expr IN rest = expr { ELet (x, EModule (x, body), rest) }
  | MODULE x = IDENT EQ body = module_expr { EModule (x, body) }
  | OPEN x = IDENT IN rest = expr { ESeq (EOpen x, rest) }
  | OPEN x = IDENT { EOpen x }
  | e = pipe_expr { e }
  ;

pipe_expr:
  | e1 = pipe_expr PIPE_GT e2 = if_expr { EApp (e2, e1) }
  | e = if_expr { e }
  ;

if_expr:
  | IF c = expr THEN t = expr ELSE f = expr { EIf (c, t, f) }
  | MATCH e = expr WITH cases = match_cases { EMatch (e, cases) }
  | TRY e = expr WITH cases = match_cases { ETry (e, cases) }
  | TYPE x = IDENT EQ ctors = ctor_defs { ETypeDef (x, [], ctors) }
  | TYPE x = IDENT params = type_params EQ ctors = ctor_defs { ETypeDef (x, params, ctors) }
  | TYPE params = type_params x = IDENT EQ ctors = ctor_defs { ETypeDef (x, params, ctors) }
  | TYPE LPAREN params = type_param_list RPAREN x = IDENT EQ ctors = ctor_defs { ETypeDef (x, params, ctors) }
  | TRAIT name = IDENT LBRACE methods = trait_methods RBRACE { ETraitDef (name, [], methods) }
  | IMPL trait_name = IDENT FOR type_name = IDENT LBRACE methods = trait_impl_methods RBRACE { ETraitImpl (trait_name, type_name, methods) }
  | WHILE c = expr DO body = expr DONE { EWhile (c, body) }
  | FUN x = IDENT ARROW body = expr { EFun (x, body) }
  | FUN UNDERSCORE ARROW body = expr { EFun ("_", body) }
  | ASSERT e = if_expr { EIf (e, ETuple [], ERaise (EString "Assertion failed")) }
  | IGNORE e = if_expr { ELet ("_", e, ETuple []) }
  | TODO s = STRING { ERaise (EString ("TODO: " ^ s)) }
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
  | e1 = cat_expr CARET e2 = range_expr { ECat (e1, e2) }
  | e = range_expr { e }
  ;

range_expr:
  | e1 = add_expr DOTDOT e2 = add_expr { ERange (e1, e2) }
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
  | e1 = app_expr e2 = postfix_expr { EApp (e1, e2) }
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
  | e = postfix_expr DOT field = IDENT { EDot (e, field) }
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
  | LBRACE e = expr WITH fields = record_fields RBRACE { ERecordUpdate (e, fields) }
  ;

record_fields:
  | f = record_field { [f] }
  | f = record_field SEMI fs = record_fields { f :: fs }
  ;

record_field:
  | name = IDENT EQ value = if_expr { (name, value) }
  | name = IDENT { (name, EVar name) }
  ;

tuple_elems:
  | e = expr COMMA es = separated_list(COMMA, expr) { e :: es }
  ;

type_params:
  | xs = nonempty_list(TYPE_VAR) { xs }
  ;

type_param_list:
  | xs = separated_list(COMMA, TYPE_VAR) { xs }
  ;

ctor_defs:
  | PIPE? defs = separated_list(PIPE, ctor_def) { defs }
  ;

ctor_def:
  | c = IDENT { (c, None) }
  | c = IDENT OF t = type_name { (c, Some t) }
  ;

type_name:
  | x = type_app { x }
  | t1 = type_app STAR t2 = type_name
      { t1 ^ " * " ^ t2 }
  ;

simple_type_name:
  | x = IDENT { x }
  | x = TYPE_VAR { x }
  | LPAREN xs = separated_list(COMMA, TYPE_VAR) RPAREN
      { "(" ^ String.concat ", " xs ^ ")" }
  | LPAREN t = type_name RPAREN { "(" ^ t ^ ")" }
  ;

type_app:
  | t = simple_type_name { t }
  | t1 = simple_type_name t2 = simple_type_name { t1 ^ " " ^ t2 }
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
  | LBRACE RBRACE { PRecord [] }
  | LBRACE fields = pattern_record_fields RBRACE { PRecord fields }
  ;

pattern_record_fields:
  | f = pattern_record_field { [f] }
  | f = pattern_record_field SEMI fs = pattern_record_fields { f :: fs }
  ;

pattern_record_field:
  | name = IDENT EQ p = pattern { (name, p) }
  | name = IDENT { (name, PVar name) }
  ;

tuple_pattern:
  | p = pattern COMMA ps = separated_list(COMMA, pattern) { p :: ps }
  ;

module_expr:
  | STRUCT e = module_body END { e }
  ;

module_body:
  | e = expr { e }
  | defs = separated_list(SEMI, module_def) { 
      match defs with
      | [] -> ETuple []
      | [d] -> d
      | d :: ds -> List.fold_left (fun acc x -> ESeq (acc, x)) d ds
    }
  ;

module_def:
  | LET x = IDENT EQ v = expr { ELet (x, v, ETuple []) }
  | LET REC x = IDENT EQ v = expr { ELetRec (x, v, ETuple []) }
  | e = expr { e }
  ;

trait_methods:
  | { [] }
  | m = trait_method SEMI ms = trait_methods { m :: ms }
  | m = trait_method { [m] }
  ;

trait_method:
  | name = IDENT COLON sig_type = IDENT { (name, sig_type) }
  ;

trait_impl_methods:
  | { [] }
  | m = trait_impl_method SEMI ms = trait_impl_methods { m :: ms }
  | m = trait_impl_method { [m] }
  ;

trait_impl_method:
  | name = IDENT EQ v = expr { (name, v) }
  ;