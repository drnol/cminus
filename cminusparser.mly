%{
open Asttype
exception Incorrect_Syntax
%}

%token <int> Tnum
%token <string> Tidentifier
%token Telse Tif Tint Treturn Tvoid Twhile
%token Tplus Tdash Ttimes Tslash 
%token Tlt Tle Tgt Tge Tequal Tne
%token Tassign Tsemicolon Tcomma 
%token Tlparen Trparen Tlbracket Trbracket Tlbrace Trbrace
%token Teof

%start program
%type<Asttype.program>program
%%
program				: declaration_list Teof { List.rev $1 }
					;
declaration_list	: declaration_list declaration	{ $2::$1 }
					| declaration	{ [$1] }
					;
declaration			: var_declaration	{ 
							match $1 with 
							VAR_DECL (typespec, name) -> VAR_DECLARATION (typespec, name)
							|ARRAY_VAR_DECL (typespec, name, index) -> ARRAY_VAR_DECLARATION (typespec, name, index)
							}
					| fun_declaration	{ $1 }
					;
var_declaration		: type_specifier Tidentifier Tsemicolon	{ VAR_DECL($1, $2) }
					| type_specifier Tidentifier Tlbracket Tnum Trbracket Tsemicolon { ARRAY_VAR_DECL($1, $2, $4) }
					;
type_specifier		: Tint	{ INT }
					| Tvoid	{ VOID }
					;
fun_declaration		: type_specifier Tidentifier Tlparen params Trparen compound_stmt {
							match $6 with
							(localDecls, stmts) -> FUN_DECLARATION ($1, $2, $4, localDecls, stmts) 
							}
					;
params				: param_list { List.rev $1 }
					| Tvoid	{ [] }
					;
param_list			: param_list Tcomma param	{ $3::$1 }
					| param	{ [$1] }
					;
param				: type_specifier Tidentifier { PARAM ($1, $2) }
					| type_specifier Tidentifier Tlbracket Trbracket { ARRAY_PARAM ($1, $2) }
					;
compound_stmt		: Tlbrace local_declarations statement_list Trbrace	{ (List.rev $2, List.rev $3) }
					;
local_declarations	: local_declarations var_declaration { $2::$1 }
					| {	[] }
					;
statement_list		: statement_list statement { 
							match $2 with
							EXPRESSION_STMT None -> $1
							|_ -> $2::$1 
					}
					| {	[] }
					;
statement			: expression_stmt	{ $1 }
					| compound_stmt		{ 
							match $1 with 
							(localDecl, stmts) -> COMPOUND_STMT (localDecl, stmts) }
					| selection_stmt	{ $1 }
					| iteration_stmt	{ $1 }
					| return_stmt		{ $1 }
					;
expression_stmt		: expression Tsemicolon	{ EXPRESSION_STMT (Some $1) }
					| Tsemicolon { EXPRESSION_STMT None	}
					;
selection_stmt		: Tif Tlparen expression Trparen statement { SELECTION_STMT ($3, $5, None) }
					| Tif Tlparen expression Trparen statement Telse statement { SELECTION_STMT ($3, $5, Some $7) }
					;
iteration_stmt		: Twhile Tlparen expression Trparen statement { ITERATION_STMT ($3, $5) }
					;
return_stmt			: Treturn Tsemicolon { RETURN_STMT None }
					| Treturn expression Tsemicolon { RETURN_STMT (Some $2) }
					;
expression			: var Tassign expression { 
							match $1 with
							(name, exprOption) -> 
								match exprOption with
								None -> ASSIGN_EXPRESSION (name, $3) 
								|Some expr -> ARRAY_ASSIGN_EXPRESSION (name, expr, $3) }
					| simple_expression { $1 }
					;
var					: Tidentifier { ($1, None) }
					| Tidentifier Tlbracket expression Trbracket { ($1, Some $3) }
					;
simple_expression	: additive_expression relop additive_expression { 
						BOOLEAN_EXPRESSION (
							match $2 with
							Tle -> LESSEQUAL ($1, $3)
							|Tlt -> LESS ($1, $3)
							|Tgt -> GREATER ($1, $3)
							|Tge -> GREATEREQUAL ($1, $3)
							|Tequal -> EQUAL ($1, $3)
							|Tne -> NOTEQUAL ($1, $3)
							|_ -> raise Incorrect_Syntax
						)						
					 }
					| additive_expression { ARITHMATIC_EXPRESSION $1 }
					;
relop				: Tle	{ Tle }
					| Tlt	{ Tlt }
					| Tgt	{ Tgt }
					| Tge	{ Tge }
					| Tequal{ Tequal }
					| Tne	{ Tne }
					;
additive_expression : additive_expression addop term {						
						match $2 with
						Tplus -> PLUS ($1, $3)
						|Tdash -> MINUS ($1, $3)
						|_ -> raise Incorrect_Syntax						
					}
					| term	{ $1 }
					;
addop				: Tplus	{ Tplus }
					| Tdash	{ Tdash }
					;
term				: term mulop factor	{					
						match $2 with
						Ttimes -> MULTIPLY ($1, $3)
						|Tslash -> DIV ($1, $3)
						|_ -> raise Incorrect_Syntax	
					}
					| factor { $1 }
					;
mulop				: Ttimes { Ttimes }
					| Tslash { Tslash }
					;
factor				: Tlparen expression Trparen { EXPRESSION $2 }
					| var	{								
						match $1 with
						(name, exprOption) -> 
							match exprOption with
							None -> VAR name
							|Some expr -> ARRAY_VAR (name, expr)
						}
					| call	{ $1 }
					| Tnum	{ NUM $1 }
					;
call				: Tidentifier Tlparen args Trparen { CALL ($1, $3) }
					;
args				: arg_list { List.rev $1 }
					| { [] }
					;
arg_list			: arg_list Tcomma expression { $3::$1 }
					| expression { [$1] }
					;