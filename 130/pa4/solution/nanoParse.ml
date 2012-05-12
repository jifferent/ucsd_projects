type token =
  | Num of (int)
  | EOF
  | TRUE
  | FALSE
  | Id of (string)
  | LET
  | REC
  | EQ
  | IN
  | FUN
  | ARROW
  | IF
  | THEN
  | ELSE
  | PLUS
  | MINUS
  | MUL
  | DIV
  | LT
  | LE
  | NE
  | AND
  | OR
  | LPAREN
  | RPAREN
  | LBRAC
  | RBRAC
  | SEMI
  | COLONCOLON

open Parsing;;
# 2 "nanoParse.mly"
(* See this for a tutorial on ocamlyacc
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano
# 38 "nanoParse.ml"
let yytransl_const = [|
    0 (* EOF *);
  258 (* TRUE *);
  259 (* FALSE *);
  261 (* LET *);
  262 (* REC *);
  263 (* EQ *);
  264 (* IN *);
  265 (* FUN *);
  266 (* ARROW *);
  267 (* IF *);
  268 (* THEN *);
  269 (* ELSE *);
  270 (* PLUS *);
  271 (* MINUS *);
  272 (* MUL *);
  273 (* DIV *);
  274 (* LT *);
  275 (* LE *);
  276 (* NE *);
  277 (* AND *);
  278 (* OR *);
  279 (* LPAREN *);
  280 (* RPAREN *);
  281 (* LBRAC *);
  282 (* RBRAC *);
  283 (* SEMI *);
  284 (* COLONCOLON *);
    0|]

let yytransl_block = [|
  257 (* Num *);
  260 (* Id *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\003\000\003\000\003\000\004\000\004\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\003\000\003\000\003\000\001\000\006\000\
\004\000\003\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\001\000\001\000\001\000\
\001\000\006\000\007\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\022\000\023\000\024\000\025\000\000\000\000\000\
\000\000\000\000\000\000\000\000\002\000\011\000\000\000\000\000\
\000\000\000\000\000\000\003\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\000\000\000\000\000\000\000\000\010\000\
\000\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\006\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yydgoto = "\002\000\
\034\000\013\000\014\000\022\000"

let yysindex = "\003\000\
\236\255\000\000\000\000\000\000\000\000\000\000\252\254\001\255\
\236\255\236\255\141\001\113\001\000\000\000\000\255\254\003\255\
\254\254\103\255\131\255\000\000\172\255\239\254\236\255\236\255\
\236\255\236\255\236\255\236\255\236\255\236\255\236\255\236\255\
\236\255\113\001\000\000\236\255\007\255\236\255\236\255\000\000\
\236\255\000\000\232\255\232\255\232\255\232\255\232\255\232\255\
\232\255\232\255\232\255\232\255\232\255\202\255\236\255\113\001\
\057\001\000\000\236\255\085\001\236\255\113\001\236\255\113\001\
\113\001"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\010\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\245\254\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\001\000\024\000\047\000\070\000\093\000\116\000\
\139\000\162\000\185\000\208\000\231\000\000\000\000\000\248\000\
\000\000\000\000\000\000\000\000\000\000\254\000\000\000\024\001\
\030\001"

let yygindex = "\000\000\
\002\000\030\000\000\000\231\255"

let yytablesize = 679
let yytable = "\015\000\
\021\000\016\000\012\000\001\000\017\000\036\000\037\000\038\000\
\042\000\028\000\018\000\019\000\021\000\055\000\007\000\058\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\012\000\
\043\000\044\000\045\000\046\000\047\000\048\000\049\000\050\000\
\051\000\052\000\053\000\000\000\000\000\054\000\000\000\056\000\
\057\000\035\000\021\000\000\000\000\000\000\000\013\000\035\000\
\035\000\000\000\035\000\000\000\000\000\000\000\000\000\000\000\
\060\000\000\000\000\000\000\000\062\000\000\000\064\000\035\000\
\065\000\000\000\000\000\000\000\000\000\014\000\000\000\000\000\
\035\000\035\000\035\000\035\000\035\000\035\000\035\000\035\000\
\035\000\035\000\035\000\035\000\000\000\035\000\035\000\000\000\
\000\000\035\000\000\000\035\000\015\000\035\000\035\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
\004\000\005\000\006\000\007\000\000\000\023\000\000\000\008\000\
\000\000\009\000\039\000\016\000\024\000\025\000\026\000\027\000\
\028\000\029\000\030\000\031\000\032\000\010\000\000\000\011\000\
\000\000\000\000\033\000\003\000\004\000\005\000\006\000\007\000\
\000\000\023\000\017\000\008\000\000\000\009\000\000\000\000\000\
\024\000\025\000\026\000\027\000\028\000\029\000\030\000\031\000\
\032\000\010\000\040\000\011\000\000\000\000\000\033\000\000\000\
\000\000\018\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\003\000\004\000\005\000\006\000\
\007\000\000\000\023\000\000\000\008\000\000\000\009\000\000\000\
\019\000\024\000\025\000\026\000\027\000\028\000\029\000\030\000\
\031\000\032\000\010\000\000\000\011\000\000\000\041\000\033\000\
\000\000\000\000\003\000\004\000\005\000\006\000\007\000\020\000\
\023\000\059\000\008\000\000\000\009\000\000\000\000\000\024\000\
\025\000\026\000\027\000\028\000\029\000\030\000\031\000\032\000\
\010\000\000\000\011\000\000\000\000\000\033\000\005\000\000\000\
\003\000\004\000\005\000\006\000\003\000\004\000\005\000\006\000\
\007\000\000\000\000\000\000\000\008\000\000\000\009\000\009\000\
\000\000\000\000\000\000\000\000\000\000\026\000\010\000\000\000\
\011\000\000\000\010\000\033\000\011\000\021\000\000\000\021\000\
\021\000\021\000\000\000\021\000\021\000\021\000\021\000\021\000\
\021\000\021\000\021\000\021\000\021\000\021\000\021\000\008\000\
\021\000\000\000\021\000\021\000\012\000\027\000\012\000\012\000\
\012\000\000\000\012\000\012\000\012\000\012\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\012\000\000\000\012\000\
\000\000\012\000\012\000\013\000\000\000\013\000\013\000\013\000\
\000\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\013\000\013\000\013\000\000\000\013\000\000\000\
\013\000\013\000\014\000\000\000\014\000\014\000\014\000\000\000\
\014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
\014\000\014\000\014\000\014\000\000\000\014\000\000\000\014\000\
\014\000\015\000\000\000\015\000\015\000\015\000\000\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
\015\000\015\000\015\000\000\000\015\000\000\000\015\000\015\000\
\016\000\000\000\016\000\016\000\016\000\000\000\016\000\016\000\
\016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
\016\000\016\000\000\000\016\000\000\000\016\000\016\000\017\000\
\000\000\017\000\017\000\017\000\000\000\017\000\017\000\017\000\
\017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
\017\000\000\000\017\000\000\000\017\000\017\000\018\000\000\000\
\018\000\018\000\018\000\000\000\018\000\018\000\018\000\018\000\
\018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
\000\000\018\000\000\000\018\000\018\000\019\000\000\000\019\000\
\019\000\019\000\000\000\019\000\019\000\019\000\019\000\019\000\
\019\000\019\000\019\000\019\000\019\000\019\000\019\000\000\000\
\019\000\000\000\019\000\019\000\020\000\000\000\020\000\020\000\
\020\000\000\000\020\000\020\000\020\000\020\000\020\000\020\000\
\020\000\020\000\020\000\020\000\020\000\020\000\000\000\020\000\
\000\000\020\000\020\000\005\000\000\000\005\000\005\000\005\000\
\000\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\005\000\000\000\005\000\009\000\
\005\000\005\000\000\000\009\000\009\000\026\000\000\000\000\000\
\000\000\026\000\026\000\000\000\000\000\000\000\000\000\009\000\
\000\000\009\000\009\000\000\000\000\000\026\000\000\000\026\000\
\026\000\000\000\000\000\000\000\000\000\000\000\000\000\008\000\
\000\000\000\000\000\000\008\000\008\000\027\000\000\000\000\000\
\000\000\027\000\027\000\000\000\000\000\000\000\000\000\008\000\
\000\000\008\000\008\000\000\000\000\000\027\000\000\000\027\000\
\027\000\003\000\004\000\005\000\006\000\007\000\000\000\023\000\
\000\000\008\000\000\000\009\000\000\000\061\000\024\000\025\000\
\026\000\027\000\028\000\029\000\030\000\031\000\032\000\010\000\
\000\000\011\000\000\000\000\000\033\000\003\000\004\000\005\000\
\006\000\007\000\000\000\023\000\063\000\008\000\000\000\009\000\
\000\000\000\000\024\000\025\000\026\000\027\000\028\000\029\000\
\030\000\031\000\032\000\010\000\000\000\011\000\000\000\000\000\
\033\000\003\000\004\000\005\000\006\000\007\000\000\000\023\000\
\000\000\008\000\000\000\009\000\000\000\000\000\024\000\025\000\
\026\000\027\000\028\000\029\000\030\000\031\000\032\000\010\000\
\000\000\011\000\000\000\000\000\033\000\003\000\004\000\005\000\
\006\000\007\000\000\000\000\000\000\000\008\000\000\000\009\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\010\000\000\000\011\000\020\000"

let yycheck = "\004\001\
\000\000\006\001\001\000\001\000\004\001\007\001\004\001\010\001\
\026\001\000\000\009\000\010\000\011\000\007\001\026\001\041\000\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
\023\000\024\000\025\000\026\000\027\000\028\000\029\000\030\000\
\031\000\032\000\033\000\255\255\255\255\036\000\255\255\038\000\
\039\000\012\000\041\000\255\255\255\255\255\255\000\000\018\000\
\019\000\255\255\021\000\255\255\255\255\255\255\255\255\255\255\
\055\000\255\255\255\255\255\255\059\000\255\255\061\000\034\000\
\063\000\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
\043\000\044\000\045\000\046\000\047\000\048\000\049\000\050\000\
\051\000\052\000\053\000\054\000\255\255\056\000\057\000\255\255\
\255\255\060\000\255\255\062\000\000\000\064\000\065\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\001\001\
\002\001\003\001\004\001\005\001\255\255\007\001\255\255\009\001\
\255\255\011\001\012\001\000\000\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\023\001\255\255\025\001\
\255\255\255\255\028\001\001\001\002\001\003\001\004\001\005\001\
\255\255\007\001\000\000\009\001\255\255\011\001\255\255\255\255\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\255\255\255\255\028\001\255\255\
\255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\001\001\002\001\003\001\004\001\
\005\001\255\255\007\001\255\255\009\001\255\255\011\001\255\255\
\000\000\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\255\255\025\001\255\255\027\001\028\001\
\255\255\255\255\001\001\002\001\003\001\004\001\005\001\000\000\
\007\001\008\001\009\001\255\255\011\001\255\255\255\255\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\023\001\255\255\025\001\255\255\255\255\028\001\000\000\255\255\
\001\001\002\001\003\001\004\001\001\001\002\001\003\001\004\001\
\005\001\255\255\255\255\255\255\009\001\255\255\011\001\000\000\
\255\255\255\255\255\255\255\255\255\255\000\000\023\001\255\255\
\025\001\255\255\023\001\028\001\025\001\005\001\255\255\007\001\
\008\001\009\001\255\255\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\000\000\
\024\001\255\255\026\001\027\001\005\001\000\000\007\001\008\001\
\009\001\255\255\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\255\255\024\001\
\255\255\026\001\027\001\005\001\255\255\007\001\008\001\009\001\
\255\255\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\255\255\024\001\255\255\
\026\001\027\001\005\001\255\255\007\001\008\001\009\001\255\255\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\255\255\024\001\255\255\026\001\
\027\001\005\001\255\255\007\001\008\001\009\001\255\255\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\255\255\024\001\255\255\026\001\027\001\
\005\001\255\255\007\001\008\001\009\001\255\255\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\255\255\024\001\255\255\026\001\027\001\005\001\
\255\255\007\001\008\001\009\001\255\255\011\001\012\001\013\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\255\255\024\001\255\255\026\001\027\001\005\001\255\255\
\007\001\008\001\009\001\255\255\011\001\012\001\013\001\014\001\
\015\001\016\001\017\001\018\001\019\001\020\001\021\001\022\001\
\255\255\024\001\255\255\026\001\027\001\005\001\255\255\007\001\
\008\001\009\001\255\255\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\255\255\
\024\001\255\255\026\001\027\001\005\001\255\255\007\001\008\001\
\009\001\255\255\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\255\255\024\001\
\255\255\026\001\027\001\005\001\255\255\007\001\008\001\009\001\
\255\255\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\019\001\020\001\021\001\022\001\255\255\024\001\008\001\
\026\001\027\001\255\255\012\001\013\001\008\001\255\255\255\255\
\255\255\012\001\013\001\255\255\255\255\255\255\255\255\024\001\
\255\255\026\001\027\001\255\255\255\255\024\001\255\255\026\001\
\027\001\255\255\255\255\255\255\255\255\255\255\255\255\008\001\
\255\255\255\255\255\255\012\001\013\001\008\001\255\255\255\255\
\255\255\012\001\013\001\255\255\255\255\255\255\255\255\024\001\
\255\255\026\001\027\001\255\255\255\255\024\001\255\255\026\001\
\027\001\001\001\002\001\003\001\004\001\005\001\255\255\007\001\
\255\255\009\001\255\255\011\001\255\255\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\255\255\025\001\255\255\255\255\028\001\001\001\002\001\003\001\
\004\001\005\001\255\255\007\001\008\001\009\001\255\255\011\001\
\255\255\255\255\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\255\255\025\001\255\255\255\255\
\028\001\001\001\002\001\003\001\004\001\005\001\255\255\007\001\
\255\255\009\001\255\255\011\001\255\255\255\255\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\255\255\025\001\255\255\255\255\028\001\001\001\002\001\003\001\
\004\001\005\001\255\255\255\255\255\255\009\001\255\255\011\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\023\001\255\255\025\001\026\001"

let yynames_const = "\
  EOF\000\
  TRUE\000\
  FALSE\000\
  LET\000\
  REC\000\
  EQ\000\
  IN\000\
  FUN\000\
  ARROW\000\
  IF\000\
  THEN\000\
  ELSE\000\
  PLUS\000\
  MINUS\000\
  MUL\000\
  DIV\000\
  LT\000\
  LE\000\
  NE\000\
  AND\000\
  OR\000\
  LPAREN\000\
  RPAREN\000\
  LBRAC\000\
  RBRAC\000\
  SEMI\000\
  COLONCOLON\000\
  "

let yynames_block = "\
  Num\000\
  Id\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Nano.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'sub_exp) in
    Obj.repr(
# 26 "nanoParse.mly"
                              ( App (_1, _2) )
# 343 "nanoParse.ml"
               : Nano.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sub_exp) in
    Obj.repr(
# 27 "nanoParse.mly"
                              ( _1 )
# 350 "nanoParse.ml"
               : Nano.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 30 "nanoParse.mly"
                              ( NilExpr )
# 356 "nanoParse.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'sub_list) in
    Obj.repr(
# 31 "nanoParse.mly"
                              ( _2 )
# 363 "nanoParse.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 32 "nanoParse.mly"
                              ( Bin (_1, Cons, _3) )
# 371 "nanoParse.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'sub_list) in
    Obj.repr(
# 35 "nanoParse.mly"
                              ( Bin (_1, Cons, _3) )
# 379 "nanoParse.ml"
               : 'sub_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 36 "nanoParse.mly"
                              ( Bin (_1, Cons, NilExpr) )
# 386 "nanoParse.ml"
               : 'sub_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : Nano.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 39 "nanoParse.mly"
                              ( If (_2, _4, _6) )
# 395 "nanoParse.ml"
               : 'sub_exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 40 "nanoParse.mly"
                              ( Fun (_2, _4) )
# 403 "nanoParse.ml"
               : 'sub_exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Nano.expr) in
    Obj.repr(
# 41 "nanoParse.mly"
                              ( _2 )
# 410 "nanoParse.ml"
               : 'sub_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'list) in
    Obj.repr(
# 42 "nanoParse.mly"
                              ( _1 )
# 417 "nanoParse.ml"
               : 'sub_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 43 "nanoParse.mly"
                              ( Bin (_1, Plus, _3) )
# 425 "nanoParse.ml"
               : 'sub_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 44 "nanoParse.mly"
                              ( Bin (_1, Minus, _3) )
# 433 "nanoParse.ml"
               : 'sub_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 45 "nanoParse.mly"
                              ( Bin (_1, Mul, _3) )
# 441 "nanoParse.ml"
               : 'sub_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 46 "nanoParse.mly"
                              ( Bin (_1, Div, _3) )
# 449 "nanoParse.ml"
               : 'sub_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 47 "nanoParse.mly"
                              ( Bin (_1, Lt, _3) )
# 457 "nanoParse.ml"
               : 'sub_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 48 "nanoParse.mly"
                              ( Bin (_1, Le, _3) )
# 465 "nanoParse.ml"
               : 'sub_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 49 "nanoParse.mly"
                              ( Bin (_1, Ne, _3) )
# 473 "nanoParse.ml"
               : 'sub_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 50 "nanoParse.mly"
                              ( Bin (_1, And, _3) )
# 481 "nanoParse.ml"
               : 'sub_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 51 "nanoParse.mly"
                              ( Bin (_1, Or, _3) )
# 489 "nanoParse.ml"
               : 'sub_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 52 "nanoParse.mly"
                              ( Bin (_1, Eq, _3) )
# 497 "nanoParse.ml"
               : 'sub_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 53 "nanoParse.mly"
                              ( Const _1 )
# 504 "nanoParse.ml"
               : 'sub_exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "nanoParse.mly"
                              ( True )
# 510 "nanoParse.ml"
               : 'sub_exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 55 "nanoParse.mly"
                              ( False )
# 516 "nanoParse.ml"
               : 'sub_exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 56 "nanoParse.mly"
                              ( Var _1 )
# 523 "nanoParse.ml"
               : 'sub_exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 57 "nanoParse.mly"
                              ( Let (_2, _4, _6) )
# 532 "nanoParse.ml"
               : 'sub_exp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Nano.expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Nano.expr) in
    Obj.repr(
# 58 "nanoParse.mly"
                              ( Letrec (_3, _5, _7) )
# 541 "nanoParse.ml"
               : 'sub_exp))
(* Entry exp *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let exp (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Nano.expr)
