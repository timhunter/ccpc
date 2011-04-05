type token =
  | ARROW
  | NEWLINE
  | EOF
  | QUOTE
  | LBRAC
  | RBRAC
  | COMMA
  | INT of (int)
  | CONCAT
  | EPSILON
  | CAT of (string)
  | TERM of (string)

open Parsing;;
# 1 "mcfgread/parse.mly"

open Rule
open Util
# 21 "mcfgread/parse.ml"
let yytransl_const = [|
  257 (* ARROW *);
  258 (* NEWLINE *);
    0 (* EOF *);
  259 (* QUOTE *);
  260 (* LBRAC *);
  261 (* RBRAC *);
  262 (* COMMA *);
  264 (* CONCAT *);
  265 (* EPSILON *);
    0|]

let yytransl_block = [|
  263 (* INT *);
  266 (* CAT *);
  267 (* TERM *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\003\000\003\000\004\000\
\004\000\005\000\005\000\005\000\005\000\006\000\000\000"

let yylen = "\002\000\
\001\000\002\000\005\000\006\000\005\000\001\000\002\000\003\000\
\004\000\001\000\001\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\015\000\000\000\000\000\002\000\000\000\
\000\000\000\000\000\000\000\000\007\000\000\000\000\000\005\000\
\000\000\000\000\000\000\000\000\000\000\003\000\004\000\000\000\
\000\000\000\000\000\000\014\000\013\000\009\000\012\000"

let yydgoto = "\002\000\
\004\000\005\000\010\000\015\000\020\000\021\000"

let yysindex = "\004\000\
\000\255\000\000\006\255\000\000\000\255\255\254\000\000\253\254\
\001\255\008\255\011\255\012\255\000\000\250\254\014\255\000\000\
\015\255\013\255\010\255\009\255\016\255\000\000\000\000\018\255\
\250\254\008\255\250\254\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\020\000\000\000\000\000\000\000\
\017\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\021\255\000\000\022\255\000\000\000\000\000\000\
\000\000\020\255\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\018\000\000\000\019\000\003\000\235\255\000\000"

let yytablesize = 29
let yytable = "\011\000\
\018\000\008\000\019\000\029\000\001\000\031\000\006\000\012\000\
\009\000\003\000\009\000\014\000\016\000\026\000\017\000\022\000\
\023\000\025\000\024\000\001\000\006\000\008\000\007\000\027\000\
\028\000\011\000\010\000\013\000\030\000"

let yycheck = "\003\001\
\007\001\003\001\009\001\025\000\001\000\027\000\001\001\011\001\
\010\001\010\001\010\001\004\001\002\001\005\001\003\001\002\001\
\002\001\008\001\006\001\000\000\004\001\002\001\005\000\008\001\
\007\001\005\001\005\001\009\000\026\000"

let yynames_const = "\
  ARROW\000\
  NEWLINE\000\
  EOF\000\
  QUOTE\000\
  LBRAC\000\
  RBRAC\000\
  COMMA\000\
  CONCAT\000\
  EPSILON\000\
  "

let yynames_block = "\
  INT\000\
  CAT\000\
  TERM\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rule) in
    Obj.repr(
# 14 "mcfgread/parse.mly"
            ([_1])
# 110 "mcfgread/parse.ml"
               : Rule.r list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'rule) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Rule.r list) in
    Obj.repr(
# 15 "mcfgread/parse.mly"
                 (_1::_2)
# 118 "mcfgread/parse.ml"
               : Rule.r list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'children) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'stringyield) in
    Obj.repr(
# 18 "mcfgread/parse.mly"
                                          (Rule.create_rule (_1, _3, _4))
# 127 "mcfgread/parse.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 19 "mcfgread/parse.mly"
                                      (Rule.create_terminating (_1, _4))
# 135 "mcfgread/parse.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    Obj.repr(
# 20 "mcfgread/parse.mly"
                                 (Rule.create_terminating (_1, " "))
# 142 "mcfgread/parse.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 23 "mcfgread/parse.mly"
       ([ _1 ])
# 149 "mcfgread/parse.ml"
               : 'children))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'children) in
    Obj.repr(
# 24 "mcfgread/parse.mly"
                (_1::_2)
# 157 "mcfgread/parse.ml"
               : 'children))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'component) in
    Obj.repr(
# 27 "mcfgread/parse.mly"
                         (Rule.create_tuplerecipe _2  )
# 164 "mcfgread/parse.ml"
               : 'stringyield))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'component) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'stringyield) in
    Obj.repr(
# 28 "mcfgread/parse.mly"
                                     ( Rule.add_to_recipe _2 _4 )
# 172 "mcfgread/parse.ml"
               : 'stringyield))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tuple) in
    Obj.repr(
# 31 "mcfgread/parse.mly"
         ( [_1] )
# 179 "mcfgread/parse.ml"
               : 'component))
; (fun __caml_parser_env ->
    Obj.repr(
# 32 "mcfgread/parse.mly"
           ( [Rule.Epsilon] )
# 185 "mcfgread/parse.ml"
               : 'component))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'tuple) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'component) in
    Obj.repr(
# 33 "mcfgread/parse.mly"
                          ( _1 :: _3 )
# 193 "mcfgread/parse.ml"
               : 'component))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'component) in
    Obj.repr(
# 34 "mcfgread/parse.mly"
                            ( Rule.Epsilon :: _3)
# 200 "mcfgread/parse.ml"
               : 'component))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 37 "mcfgread/parse.mly"
                 ( Rule.Component(_1,_3) )
# 208 "mcfgread/parse.ml"
               : 'tuple))
(* Entry mcfgrule *)
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
let mcfgrule (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Rule.r list)
