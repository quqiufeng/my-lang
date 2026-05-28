(* 基础语言模板 - 语法分析器 *)

%{
open Ast
%}

%token <int> INT
%token <string> IDENT
%token <string> STRING
%token TRUE FALSE
%token LET IN FUN IF THEN ELSE MATCH WITH
%token PLUS MINUS STAR SLASH
%token EQ NEQ LT GT LE GE
%token AND OR NOT
%token LPAREN RPAREN LBRACKET RBRACKET
%token CONS COMMA SEMI PIPE ARROW UNIT
%token EOF

%left OR
%left AND
%left EQ NEQ LT GT LE GE
%left PLUS MINUS
%left STAR SLASH
%right CONS
%right NOT
%left LPAREN LBRACKET

%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e }

expr:
  | let_expr { $1 }
  | seq_expr { $1 }

let_expr:
  | LET; x = IDENT; EQ; e1 = expr; IN; e2 = expr
    { ELet (x, e1, e2) }
  | LET; REC; f = IDENT; params = nonempty_list(IDENT); EQ; body = expr; IN; e2 = expr
    { ELet (f, EFun (params, body), e2) }
  | fun_expr { $1 }

fun_expr:
  | FUN; params = nonempty_list(IDENT); ARROW; body = expr
    { EFun (params, body) }
  | if_expr { $1 }

if_expr:
  | IF; cond = expr; THEN; then_ = expr; ELSE; else_ = expr
    { EIf (cond, then_, else_) }
  | match_expr { $1 }

match_expr:
  | MATCH; e = expr; WITH; cases = match_cases
    { EMatch (e, cases) }
  | binary_expr { $1 }

match_cases:
  | PIPE? cases = separated_list(PIPE, match_case) { cases }

match_case:
  | p = pattern; ARROW; e = expr { (p, e) }

pattern:
  | x = IDENT { PVar x }
  | n = INT { PInt n }
  | TRUE { PBool true }
  | FALSE { PBool false }
  | UNIT { PVar "_" }
  | LPAREN; ps = separated_list(COMMA, pattern); RPAREN
    { match ps with [p] -> p | _ -> PTuple ps }
  | LBRACKET; ps = separated_list(COMMA, pattern); RBRACKET
    { PList ps }
  | WILD { PWild }

binary_expr:
  | e1 = binary_expr; op = binop; e2 = binary_expr { EBinary (op, e1, e2) }
  | unary_expr { $1 }

%inline binop:
  | PLUS { Add }
  | MINUS { Sub }
  | STAR { Mul }
  | SLASH { Div }
  | EQ { Eq }
  | NEQ { Neq }
  | LT { Lt }
  | LE { Le }
  | GT { Gt }
  | GE { Ge }
  | AND { And }
  | OR { Or }

unary_expr:
  | MINUS; e = unary_expr { EUnary (Neg, e) }
  | NOT; e = unary_expr { EUnary (Not, e) }
  | app_expr { $1 }

app_expr:
  | f = app_expr; LPAREN; args = separated_list(COMMA, expr); RPAREN
    { EApp (f, args) }
  | primary_expr { $1 }

primary_expr:
  | n = INT { EInt n }
  | TRUE { EBool true }
  | FALSE { EBool false }
  | s = STRING { EString s }
  | UNIT { EUnit }
  | x = IDENT { EVar x }
  | LPAREN; es = separated_list(COMMA, expr); RPAREN
    { match es with [e] -> e | _ -> ETuple es }
  | LBRACKET; es = separated_list(COMMA, expr); RBRACKET
    { EList es }
