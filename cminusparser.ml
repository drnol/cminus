type token =
  | Tnum of (int)
  | Tidentifier of (string)
  | Telse
  | Tif
  | Tint
  | Treturn
  | Tvoid
  | Twhile
  | Tplus
  | Tdash
  | Ttimes
  | Tslash
  | Tlt
  | Tle
  | Tgt
  | Tge
  | Tequal
  | Tne
  | Tassign
  | Tsemicolon
  | Tcomma
  | Tlparen
  | Trparen
  | Tlbracket
  | Trbracket
  | Tlbrace
  | Trbrace
  | Teof

open Parsing;;
# 2 "cminusparser.mly"
open Asttype
exception Incorrect_Syntax
# 36 "cminusparser.ml"
let yytransl_const = [|
  259 (* Telse *);
  260 (* Tif *);
  261 (* Tint *);
  262 (* Treturn *);
  263 (* Tvoid *);
  264 (* Twhile *);
  265 (* Tplus *);
  266 (* Tdash *);
  267 (* Ttimes *);
  268 (* Tslash *);
  269 (* Tlt *);
  270 (* Tle *);
  271 (* Tgt *);
  272 (* Tge *);
  273 (* Tequal *);
  274 (* Tne *);
  275 (* Tassign *);
  276 (* Tsemicolon *);
  277 (* Tcomma *);
  278 (* Tlparen *);
  279 (* Trparen *);
  280 (* Tlbracket *);
  281 (* Trbracket *);
  282 (* Tlbrace *);
  283 (* Trbrace *);
  284 (* Teof *);
    0|]

let yytransl_block = [|
  257 (* Tnum *);
  258 (* Tidentifier *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\004\000\004\000\006\000\
\006\000\005\000\007\000\007\000\009\000\009\000\010\000\010\000\
\008\000\011\000\011\000\012\000\012\000\013\000\013\000\013\000\
\013\000\013\000\014\000\014\000\015\000\015\000\016\000\017\000\
\017\000\018\000\018\000\019\000\019\000\020\000\020\000\022\000\
\022\000\022\000\022\000\022\000\022\000\021\000\021\000\023\000\
\023\000\024\000\024\000\025\000\025\000\026\000\026\000\026\000\
\026\000\027\000\028\000\028\000\029\000\029\000\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\001\000\001\000\003\000\006\000\001\000\
\001\000\006\000\001\000\001\000\003\000\001\000\002\000\004\000\
\004\000\002\000\000\000\002\000\000\000\001\000\001\000\001\000\
\001\000\001\000\002\000\001\000\005\000\007\000\005\000\002\000\
\003\000\003\000\001\000\001\000\004\000\003\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\003\000\001\000\001\000\
\001\000\003\000\001\000\001\000\001\000\003\000\001\000\001\000\
\001\000\004\000\001\000\000\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\008\000\009\000\063\000\000\000\003\000\004\000\
\005\000\000\000\001\000\002\000\000\000\006\000\000\000\000\000\
\000\000\000\000\000\000\000\000\014\000\000\000\000\000\000\000\
\000\000\000\000\000\000\019\000\010\000\013\000\007\000\016\000\
\000\000\018\000\000\000\000\000\000\000\057\000\000\000\000\000\
\000\000\000\000\028\000\000\000\017\000\023\000\020\000\022\000\
\024\000\025\000\026\000\000\000\000\000\035\000\000\000\000\000\
\051\000\056\000\000\000\000\000\000\000\032\000\000\000\000\000\
\000\000\027\000\000\000\048\000\049\000\041\000\040\000\042\000\
\043\000\044\000\045\000\000\000\000\000\052\000\053\000\000\000\
\062\000\000\000\000\000\000\000\000\000\033\000\000\000\054\000\
\034\000\055\000\000\000\000\000\050\000\058\000\000\000\037\000\
\000\000\000\000\061\000\000\000\031\000\000\000\030\000"

let yydgoto = "\002\000\
\005\000\006\000\007\000\008\000\009\000\010\000\019\000\046\000\
\020\000\021\000\033\000\036\000\047\000\048\000\049\000\050\000\
\051\000\052\000\053\000\054\000\055\000\076\000\077\000\056\000\
\080\000\057\000\058\000\082\000\083\000"

let yysindex = "\020\000\
\009\255\000\000\000\000\000\000\000\000\010\255\000\000\000\000\
\000\000\031\255\000\000\000\000\244\254\000\000\066\255\039\255\
\000\000\056\255\021\255\044\255\000\000\037\255\057\255\040\255\
\009\255\060\255\064\255\000\000\000\000\000\000\000\000\000\000\
\009\255\000\000\090\255\005\255\004\255\000\000\053\255\088\255\
\028\255\100\255\000\000\000\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\105\255\089\255\000\000\069\255\249\254\
\000\000\000\000\000\255\000\255\000\255\000\000\107\255\000\255\
\108\255\000\000\000\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\255\000\255\000\000\000\000\000\255\
\000\000\109\255\118\255\117\255\121\255\000\000\125\255\000\000\
\000\000\000\000\042\255\249\254\000\000\000\000\000\255\000\000\
\068\255\068\255\000\000\146\255\000\000\068\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\034\255\000\000\000\000\133\255\000\000\000\000\070\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\033\255\000\000\000\000\000\000\000\000\000\000\086\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\103\255\000\000\143\255\120\255\
\000\000\000\000\136\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\138\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\149\255\137\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\041\255\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\159\000\134\000\000\000\031\000\000\000\147\000\
\000\000\148\000\000\000\000\000\078\000\000\000\000\000\000\000\
\000\000\215\255\101\000\000\000\103\000\000\000\000\000\105\000\
\000\000\104\000\000\000\000\000\000\000"

let yytablesize = 184
let yytable = "\063\000\
\038\000\039\000\065\000\078\000\079\000\038\000\039\000\014\000\
\040\000\015\000\041\000\016\000\042\000\003\000\003\000\004\000\
\004\000\081\000\084\000\085\000\001\000\044\000\087\000\014\000\
\043\000\089\000\044\000\016\000\038\000\039\000\028\000\045\000\
\013\000\021\000\021\000\009\000\021\000\011\000\021\000\022\000\
\021\000\029\000\029\000\024\000\029\000\018\000\029\000\062\000\
\029\000\044\000\068\000\069\000\021\000\099\000\021\000\018\000\
\012\000\023\000\021\000\021\000\029\000\026\000\029\000\035\000\
\025\000\028\000\029\000\029\000\038\000\039\000\003\000\040\000\
\017\000\041\000\059\000\042\000\060\000\068\000\069\000\031\000\
\027\000\070\000\071\000\072\000\073\000\074\000\075\000\043\000\
\032\000\044\000\015\000\037\000\015\000\028\000\036\000\036\000\
\036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
\036\000\036\000\036\000\067\000\036\000\061\000\036\000\055\000\
\055\000\055\000\055\000\055\000\055\000\055\000\055\000\055\000\
\055\000\064\000\055\000\055\000\066\000\055\000\086\000\055\000\
\047\000\047\000\088\000\094\000\047\000\047\000\047\000\047\000\
\047\000\047\000\095\000\047\000\047\000\096\000\047\000\097\000\
\047\000\046\000\046\000\098\000\102\000\046\000\046\000\046\000\
\046\000\046\000\046\000\011\000\046\000\046\000\060\000\046\000\
\059\000\046\000\039\000\039\000\012\000\039\000\034\000\039\000\
\038\000\038\000\029\000\038\000\030\000\038\000\100\000\101\000\
\090\000\090\000\091\000\103\000\090\000\092\000\000\000\093\000"

let yycheck = "\041\000\
\001\001\002\001\044\000\011\001\012\001\001\001\002\001\020\001\
\004\001\022\001\006\001\024\001\008\001\005\001\005\001\007\001\
\007\001\059\000\060\000\061\000\001\000\022\001\064\000\020\001\
\020\001\067\000\022\001\024\001\001\001\002\001\026\001\027\001\
\002\001\001\001\002\001\002\001\004\001\028\001\006\001\001\001\
\008\001\001\001\002\001\023\001\004\001\015\000\006\001\020\001\
\008\001\022\001\009\001\010\001\020\001\095\000\022\001\025\000\
\023\001\002\001\026\001\027\001\020\001\025\001\022\001\033\000\
\021\001\026\001\026\001\027\001\001\001\002\001\005\001\004\001\
\007\001\006\001\022\001\008\001\024\001\009\001\010\001\020\001\
\024\001\013\001\014\001\015\001\016\001\017\001\018\001\020\001\
\025\001\022\001\021\001\002\001\023\001\026\001\009\001\010\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\019\001\023\001\022\001\025\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\022\001\020\001\021\001\020\001\023\001\020\001\025\001\
\009\001\010\001\023\001\023\001\013\001\014\001\015\001\016\001\
\017\001\018\001\021\001\020\001\021\001\025\001\023\001\023\001\
\025\001\009\001\010\001\023\001\003\001\013\001\014\001\015\001\
\016\001\017\001\018\001\023\001\020\001\021\001\023\001\023\001\
\023\001\025\001\020\001\021\001\006\000\023\001\033\000\025\001\
\020\001\021\001\024\000\023\001\025\000\025\001\097\000\098\000\
\076\000\077\000\076\000\102\000\080\000\077\000\255\255\080\000"

let yynames_const = "\
  Telse\000\
  Tif\000\
  Tint\000\
  Treturn\000\
  Tvoid\000\
  Twhile\000\
  Tplus\000\
  Tdash\000\
  Ttimes\000\
  Tslash\000\
  Tlt\000\
  Tle\000\
  Tgt\000\
  Tge\000\
  Tequal\000\
  Tne\000\
  Tassign\000\
  Tsemicolon\000\
  Tcomma\000\
  Tlparen\000\
  Trparen\000\
  Tlbracket\000\
  Trbracket\000\
  Tlbrace\000\
  Trbrace\000\
  Teof\000\
  "

let yynames_block = "\
  Tnum\000\
  Tidentifier\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'declaration_list) in
    Obj.repr(
# 18 "cminusparser.mly"
                                   ( List.rev _1 )
# 240 "cminusparser.ml"
               : Asttype.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'declaration_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'declaration) in
    Obj.repr(
# 20 "cminusparser.mly"
                                                ( _2::_1 )
# 248 "cminusparser.ml"
               : 'declaration_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'declaration) in
    Obj.repr(
# 21 "cminusparser.mly"
                   ( [_1] )
# 255 "cminusparser.ml"
               : 'declaration_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var_declaration) in
    Obj.repr(
# 23 "cminusparser.mly"
                                ( 
							match _1 with 
							VAR_DECL (typespec, name) -> VAR_DECLARATION (typespec, name)
							|ARRAY_VAR_DECL (typespec, name, index) -> ARRAY_VAR_DECLARATION (typespec, name, index)
							)
# 266 "cminusparser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fun_declaration) in
    Obj.repr(
# 28 "cminusparser.mly"
                       ( _1 )
# 273 "cminusparser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'type_specifier) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 30 "cminusparser.mly"
                                                         ( VAR_DECL(_1, _2) )
# 281 "cminusparser.ml"
               : 'var_declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'type_specifier) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : int) in
    Obj.repr(
# 31 "cminusparser.mly"
                                                                      ( ARRAY_VAR_DECL(_1, _2, _4) )
# 290 "cminusparser.ml"
               : 'var_declaration))
; (fun __caml_parser_env ->
    Obj.repr(
# 33 "cminusparser.mly"
                       ( INT )
# 296 "cminusparser.ml"
               : 'type_specifier))
; (fun __caml_parser_env ->
    Obj.repr(
# 34 "cminusparser.mly"
             ( VOID )
# 302 "cminusparser.ml"
               : 'type_specifier))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'type_specifier) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'params) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'compound_stmt) in
    Obj.repr(
# 36 "cminusparser.mly"
                                                                                   (
							match _6 with
							(localDecls, stmts) -> FUN_DECLARATION (_1, _2, _4, localDecls, stmts) 
							)
# 315 "cminusparser.ml"
               : 'fun_declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param_list) in
    Obj.repr(
# 41 "cminusparser.mly"
                       ( List.rev _1 )
# 322 "cminusparser.ml"
               : 'params))
; (fun __caml_parser_env ->
    Obj.repr(
# 42 "cminusparser.mly"
             ( [] )
# 328 "cminusparser.ml"
               : 'params))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'param_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 44 "cminusparser.mly"
                                       ( _3::_1 )
# 336 "cminusparser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'param) in
    Obj.repr(
# 45 "cminusparser.mly"
             ( [_1] )
# 343 "cminusparser.ml"
               : 'param_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'type_specifier) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 47 "cminusparser.mly"
                                      ( PARAM (_1, _2) )
# 351 "cminusparser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'type_specifier) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 48 "cminusparser.mly"
                                                      ( ARRAY_PARAM (_1, _2) )
# 359 "cminusparser.ml"
               : 'param))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'local_declarations) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list) in
    Obj.repr(
# 50 "cminusparser.mly"
                                                                   ( (List.rev _2, List.rev _3) )
# 367 "cminusparser.ml"
               : 'compound_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'local_declarations) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'var_declaration) in
    Obj.repr(
# 52 "cminusparser.mly"
                                                        ( _2::_1 )
# 375 "cminusparser.ml"
               : 'local_declarations))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "cminusparser.mly"
       (	[] )
# 381 "cminusparser.ml"
               : 'local_declarations))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statement_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 55 "cminusparser.mly"
                                           ( 
							match _2 with
							EXPRESSION_STMT None -> _1
							|_ -> _2::_1 
					)
# 393 "cminusparser.ml"
               : 'statement_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "cminusparser.mly"
       (	[] )
# 399 "cminusparser.ml"
               : 'statement_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression_stmt) in
    Obj.repr(
# 62 "cminusparser.mly"
                              ( _1 )
# 406 "cminusparser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'compound_stmt) in
    Obj.repr(
# 63 "cminusparser.mly"
                      ( 
							match _1 with 
							(localDecl, stmts) -> COMPOUND_STMT (localDecl, stmts) )
# 415 "cminusparser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'selection_stmt) in
    Obj.repr(
# 66 "cminusparser.mly"
                      ( _1 )
# 422 "cminusparser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'iteration_stmt) in
    Obj.repr(
# 67 "cminusparser.mly"
                      ( _1 )
# 429 "cminusparser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'return_stmt) in
    Obj.repr(
# 68 "cminusparser.mly"
                    ( _1 )
# 436 "cminusparser.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 70 "cminusparser.mly"
                                         ( EXPRESSION_STMT (Some _1) )
# 443 "cminusparser.ml"
               : 'expression_stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "cminusparser.mly"
                  ( EXPRESSION_STMT None	)
# 449 "cminusparser.ml"
               : 'expression_stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 73 "cminusparser.mly"
                                                           ( SELECTION_STMT (_3, _5, None) )
# 457 "cminusparser.ml"
               : 'selection_stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'statement) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 74 "cminusparser.mly"
                                                                ( SELECTION_STMT (_3, _5, Some _7) )
# 466 "cminusparser.ml"
               : 'selection_stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expression) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 76 "cminusparser.mly"
                                                              ( ITERATION_STMT (_3, _5) )
# 474 "cminusparser.ml"
               : 'iteration_stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "cminusparser.mly"
                                   ( RETURN_STMT None )
# 480 "cminusparser.ml"
               : 'return_stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 79 "cminusparser.mly"
                                     ( RETURN_STMT (Some _2) )
# 487 "cminusparser.ml"
               : 'return_stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 81 "cminusparser.mly"
                                      ( 
							match _1 with
							(name, exprOption) -> 
								match exprOption with
								None -> ASSIGN_EXPRESSION (name, _3) 
								|Some expr -> ARRAY_ASSIGN_EXPRESSION (name, expr, _3) )
# 500 "cminusparser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'simple_expression) in
    Obj.repr(
# 87 "cminusparser.mly"
                         ( _1 )
# 507 "cminusparser.ml"
               : 'expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 89 "cminusparser.mly"
                      ( (_1, None) )
# 514 "cminusparser.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 90 "cminusparser.mly"
                                                  ( (_1, Some _3) )
# 522 "cminusparser.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'additive_expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'relop) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'additive_expression) in
    Obj.repr(
# 92 "cminusparser.mly"
                                                                  ( 
						BOOLEAN_EXPRESSION (
							match _2 with
							Tle -> LESSEQUAL (_1, _3)
							|Tlt -> LESS (_1, _3)
							|Tgt -> GREATER (_1, _3)
							|Tge -> GREATEREQUAL (_1, _3)
							|Tequal -> EQUAL (_1, _3)
							|Tne -> NOTEQUAL (_1, _3)
							|_ -> raise Incorrect_Syntax
						)						
					 )
# 542 "cminusparser.ml"
               : 'simple_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'additive_expression) in
    Obj.repr(
# 104 "cminusparser.mly"
                           ( ARITHMATIC_EXPRESSION _1 )
# 549 "cminusparser.ml"
               : 'simple_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 106 "cminusparser.mly"
               ( Tle )
# 555 "cminusparser.ml"
               : 'relop))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "cminusparser.mly"
           ( Tlt )
# 561 "cminusparser.ml"
               : 'relop))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "cminusparser.mly"
           ( Tgt )
# 567 "cminusparser.ml"
               : 'relop))
; (fun __caml_parser_env ->
    Obj.repr(
# 109 "cminusparser.mly"
           ( Tge )
# 573 "cminusparser.ml"
               : 'relop))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "cminusparser.mly"
             ( Tequal )
# 579 "cminusparser.ml"
               : 'relop))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "cminusparser.mly"
           ( Tne )
# 585 "cminusparser.ml"
               : 'relop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'additive_expression) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'addop) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 113 "cminusparser.mly"
                                                     (						
						match _2 with
						Tplus -> PLUS (_1, _3)
						|Tdash -> MINUS (_1, _3)
						|_ -> raise Incorrect_Syntax						
					)
# 599 "cminusparser.ml"
               : 'additive_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 119 "cminusparser.mly"
            ( _1 )
# 606 "cminusparser.ml"
               : 'additive_expression))
; (fun __caml_parser_env ->
    Obj.repr(
# 121 "cminusparser.mly"
                 ( Tplus )
# 612 "cminusparser.ml"
               : 'addop))
; (fun __caml_parser_env ->
    Obj.repr(
# 122 "cminusparser.mly"
             ( Tdash )
# 618 "cminusparser.ml"
               : 'addop))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'mulop) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 124 "cminusparser.mly"
                            (					
						match _2 with
						Ttimes -> MULTIPLY (_1, _3)
						|Tslash -> DIV (_1, _3)
						|_ -> raise Incorrect_Syntax	
					)
# 632 "cminusparser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'factor) in
    Obj.repr(
# 130 "cminusparser.mly"
              ( _1 )
# 639 "cminusparser.ml"
               : 'term))
; (fun __caml_parser_env ->
    Obj.repr(
# 132 "cminusparser.mly"
                  ( Ttimes )
# 645 "cminusparser.ml"
               : 'mulop))
; (fun __caml_parser_env ->
    Obj.repr(
# 133 "cminusparser.mly"
              ( Tslash )
# 651 "cminusparser.ml"
               : 'mulop))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expression) in
    Obj.repr(
# 135 "cminusparser.mly"
                                       ( EXPRESSION _2 )
# 658 "cminusparser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'var) in
    Obj.repr(
# 136 "cminusparser.mly"
           (								
						match _1 with
						(name, exprOption) -> 
							match exprOption with
							None -> VAR name
							|Some expr -> ARRAY_VAR (name, expr)
						)
# 671 "cminusparser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'call) in
    Obj.repr(
# 143 "cminusparser.mly"
            ( _1 )
# 678 "cminusparser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 144 "cminusparser.mly"
            ( NUM _1 )
# 685 "cminusparser.ml"
               : 'factor))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args) in
    Obj.repr(
# 146 "cminusparser.mly"
                                           ( CALL (_1, _3) )
# 693 "cminusparser.ml"
               : 'call))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arg_list) in
    Obj.repr(
# 148 "cminusparser.mly"
                   ( List.rev _1 )
# 700 "cminusparser.ml"
               : 'args))
; (fun __caml_parser_env ->
    Obj.repr(
# 149 "cminusparser.mly"
       ( [] )
# 706 "cminusparser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arg_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 151 "cminusparser.mly"
                                        ( _3::_1 )
# 714 "cminusparser.ml"
               : 'arg_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expression) in
    Obj.repr(
# 152 "cminusparser.mly"
                  ( [_1] )
# 721 "cminusparser.ml"
               : 'arg_list))
(* Entry program *)
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
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Asttype.program)
