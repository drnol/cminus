type type_specifier = INT | VOID

type program = declaration list
and declaration = VAR_DECLARATION of type_specifier * string
				|ARRAY_VAR_DECLARATION of type_specifier * string * int  
				|FUN_DECLARATION of type_specifier * string * param list * var_declaration list * statement list
and var_declaration = VAR_DECL of type_specifier * string 
				|ARRAY_VAR_DECL of type_specifier * string * int
and param = PARAM of type_specifier * string
			|ARRAY_PARAM of type_specifier * string
and statement = EXPRESSION_STMT of expression option
				|COMPOUND_STMT of var_declaration list * statement list
				|SELECTION_STMT of expression * statement * statement option
				|ITERATION_STMT of expression * statement
				|RETURN_STMT of expression option
and expression = ASSIGN_EXPRESSION of string * expression
				|ARRAY_ASSIGN_EXPRESSION of string * expression * expression
				|ARITHMATIC_EXPRESSION of arithmatic_expression	(* 	simple-expression	*)
				|BOOLEAN_EXPRESSION of boolean_expression		(* 						*)
and arithmatic_expression = PLUS of arithmatic_expression * arithmatic_expression
							|MINUS of arithmatic_expression * arithmatic_expression
							|MULTIPLY of arithmatic_expression * arithmatic_expression
							|DIV of arithmatic_expression * arithmatic_expression
							|NUM of int
							|VAR of string
							|ARRAY_VAR of string * expression
							|EXPRESSION of expression
							|CALL of string * expression list
and boolean_expression = LESSEQUAL of arithmatic_expression * arithmatic_expression
						|LESS of arithmatic_expression * arithmatic_expression
						|GREATER of arithmatic_expression * arithmatic_expression
						|GREATEREQUAL of arithmatic_expression * arithmatic_expression
						|EQUAL of arithmatic_expression * arithmatic_expression
						|NOTEQUAL of arithmatic_expression * arithmatic_expression