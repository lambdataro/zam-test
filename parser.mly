/*========================================*
 * ZAM テスト実装
 * parser.mly
 *========================================*/

%{
open Syntax
%}

%token <int> INT
%token <string> ID

%token PLS MNS AST LE LT
%token EQ ARROW LP RP
%token IF THEN ELSE
%token LET REC IN FUN
%token EOF

%left IN ARROW
%right ELSE
%nonassoc LT LE
%left PLS MNS
%left AST
%left APP
%nonassoc UNARY
%nonassoc ID INT LP

%type <Syntax.expr> main
%start main

%%

main:
  | expr EOF
    { $1 }
;

arg_expr:
  | ID
    { Var $1 }
  | INT
    { Int $1 }
  | LP expr RP
    { $2 }
;

arg_expr_list:
  | arg_expr
    { [$1] }
  | arg_expr_list arg_expr
    { $2 :: $1 }
;

expr:
  | arg_expr
    { $1 }
  | expr arg_expr_list %prec APP
    { App ($1 :: List.rev $2) }
  | FUN ID ARROW expr
    { Fun ($2, $4) }
  | expr PLS expr
    { BinApp (Add, $1, $3) }
  | expr MNS expr
    { BinApp (Sub, $1, $3)}
  | expr AST expr
    { BinApp (Mul, $1, $3) }
  | expr LE expr
    { BinApp (Le, $1, $3) } 
  | expr LT expr
    { BinApp (Lt, $1, $3) } 
  | LET REC ID EQ expr IN expr
    { LetRec ($3, $5, $7) }
  | IF expr THEN expr ELSE expr
    { If ($2, $4, $6) }
;

