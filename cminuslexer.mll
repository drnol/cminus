{ 
open Cminusparser
exception Eof
}

let digit = ['0'-'9']
let letter = ['A'-'Z' 'a'-'z']
let whitechar = [' ' '\t' '\r' '\n']

rule cminustoken = parse
	whitechar	{ cminustoken lexbuf }
	|"else"		{ Telse }
	|"if"		{ Tif }
	|"int"		{ Tint }
	|"return" 	{ Treturn }
	|"void"		{ Tvoid }
	|"while"	{ Twhile }
	|'+'		{ Tplus }
	|'-'		{ Tdash }
	|'*'		{ Ttimes }
	|'/'		{ Tslash }
	|'<'		{ Tlt }
	|"<="		{ Tle }
	|'>'		{ Tgt }
	|">="		{ Tge }
	|"=="		{ Tequal }
	|"!="		{ Tne }
	|'='		{ Tassign }
	|';'		{ Tsemicolon }
	|','		{ Tcomma }
	|'('		{ Tlparen }
	|')'		{ Trparen }
	|'['		{ Tlbracket }
	|']'		{ Trbracket }
	|'{'		{ Tlbrace }
	|'}'		{ Trbrace }
	|"/*"(('/'*('*'*[^'*''/']'/'*)*'*'*) as lexeme)"*/" { cminustoken lexbuf }
	|digit+ as lexeme { Tnum (int_of_string lexeme) }
	|letter+ as lexeme 	{ Tidentifier lexeme }
	|eof		{ Teof }