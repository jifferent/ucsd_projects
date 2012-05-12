%{
(* See this for a tutorial on ocamlyacc
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano
%}

%token <int> Num
%token EOF
%token TRUE FALSE
%token <string> Id
%token LET REC EQ IN FUN ARROW IF THEN ELSE
%token PLUS MINUS MUL DIV LT LE NE AND OR
%token LPAREN RPAREN
%token LBRAC RBRAC SEMI COLONCOLON

%nonassoc LET FUN IF
%left EQ LT LE NE OR AND MINUS PLUS MUL DIV APPLY
%right COLONCOLON

%start exp
%type <Nano.expr> exp

%%

exp:
  | exp sub_exp %prec APPLY   { App ($1, $2) }
  | sub_exp                   { $1 }

list:
  | LBRAC RBRAC               { NilExpr }
  | LBRAC sub_list RBRAC      { $2 }
  | exp COLONCOLON exp        { Bin ($1, Cons, $3) }

sub_list:
  | exp SEMI sub_list         { Bin ($1, Cons, $3) }
  | exp                       { Bin ($1, Cons, NilExpr) }

sub_exp:
  | IF exp THEN exp ELSE exp  { If ($2, $4, $6) }
  | FUN Id ARROW exp          { Fun ($2, $4) }
  | LPAREN exp RPAREN         { $2 }
  | list                      { $1 }
  | exp PLUS  exp             { Bin ($1, Plus, $3) }
  | exp MINUS exp             { Bin ($1, Minus, $3) }
  | exp MUL   exp             { Bin ($1, Mul, $3) }
  | exp DIV   exp             { Bin ($1, Div, $3) }
  | exp LT    exp             { Bin ($1, Lt, $3) }
  | exp LE    exp             { Bin ($1, Le, $3) }
  | exp NE    exp             { Bin ($1, Ne, $3) }
  | exp AND   exp             { Bin ($1, And, $3) }
  | exp OR    exp             { Bin ($1, Or, $3) }
  | exp EQ    exp             { Bin ($1, Eq, $3) }
  | Num                       { Const $1 }
  | TRUE                      { True }
  | FALSE                     { False }
  | Id                        { Var $1 }
  | LET Id EQ exp IN exp      { Let ($2, $4, $6) }
  | LET REC Id EQ exp IN exp  { Letrec ($3, $5, $7) }
