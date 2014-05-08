open Asttype

(* auxiliary procedure *)

let rec whiteSpace = function 
	0 -> "   "
	|i -> "   "^whiteSpace (i-1)
	
let print ~depth = print_string (whiteSpace depth); Printf.printf 

let printForm ~depth ~tag ~printee =
		print ~depth "%s ---->\n" tag;
		printee None;	
		print ~depth "<---- end of %s\n" tag

let rec printList ~depth ~printer = function
	[] -> ()
	|h::t -> printer ~depth h; printList ~depth ~printer t
	
(* printing proedure *)

let typeString = function
	VOID -> "void"
	|INT -> "int"
let rec printAST root = 
		printForm ~depth:0 ~tag:"program" ~printee:
		(fun _ -> (printList ~depth:1 ~printer:printDecl root))
		
and printDecl ~depth = function
	VAR_DECLARATION (typespec, name) -> 
		printForm ~depth ~tag:"VAR_DECLARATION" ~printee:
		(fun _ -> 
			print ~depth:(depth+1) "type: %s\n" (typeString typespec);
			print ~depth:(depth+1) "name: %s\n" name
		)
	|ARRAY_VAR_DECLARATION (typespec, name, index) -> 
		printForm ~depth ~tag:"ARRAY_VAR_DECLARATION" ~printee:
		(fun _ -> 
			print ~depth:(depth+1) "type: %s\n" (typeString typespec);
			print ~depth:(depth+1) "name: %s\n" name;
			print ~depth:(depth+1) "index: %d\n" index
		)
	|FUN_DECLARATION (typespec, name, params, local_decls, stmts) -> 
		printForm ~depth ~tag:"FUN_DECLARATION" ~printee:
		(fun _ -> 
			print ~depth:(depth+1) "return type: %s\n" (typeString typespec);
			print ~depth:(depth+1) "name: %s\n" (typeString typespec);	
			printList ~depth:(depth+1) ~printer:printParam		params;
			printList ~depth:(depth+1) ~printer:printVarDecl	local_decls;
			printList ~depth:(depth+1) ~printer:printStmt		stmts
		); print_string "\n"
and printVarDecl ~depth = function
	VAR_DECL (typespec, name) -> 
		printForm ~depth ~tag:"VAR_DECL" ~printee:
		(fun _ -> 
			print ~depth:(depth+1) "type: %s\n" (typeString typespec);
			print ~depth:(depth+1) "name: %s\n" name
		)
	|ARRAY_VAR_DECL (typespec, name, index) -> 
		printForm ~depth ~tag:"ARRAY_VAR_DECL" ~printee:
		(fun _ -> 
			print ~depth:(depth+1) "type: %s\n" (typeString typespec);
			print ~depth:(depth+1) "name: %s\n" name;
			print ~depth:(depth+1) "index: %d\n" index
		)
and printParam ~depth = function
	PARAM (typespec, name) ->
		printForm ~depth ~tag:"PARAM" ~printee:
		(fun _ -> 
			print ~depth:(depth+1) "type: %s\n" (typeString typespec);
			print ~depth:(depth+1) "name: %s\n" name
		)
	|ARRAY_PARAM (typespec, name) ->
		printForm ~depth ~tag:"ARRAY_PARAM" ~printee:
		(fun _ -> 
			print ~depth:(depth+1) "type: %s\n" (typeString typespec);
			print ~depth:(depth+1) "name: %s\n" name
		)
and printStmt ~depth = function
	EXPRESSION_STMT exprOption -> 
		printForm ~depth ~tag:"EXPRESSION_STMT" ~printee:
		(fun _ -> 
			match exprOption with
				None -> ()
				|Some expr -> printExpr ~depth:(depth+1) expr
		)
	|COMPOUND_STMT (local_decls, stmts) -> 
		printForm ~depth ~tag:"COMPOUND_STMT" ~printee:
		(fun _ -> 
			printList	~depth:(depth+1) ~printer:printVarDecl	local_decls;
			printList	~depth:(depth+1) ~printer:printStmt		stmts
		)
	|SELECTION_STMT (condExpr, thenStmt, elseStmt) -> 
		printForm ~depth ~tag:"SELECTION_STMT" ~printee:
		(fun _ -> 
			printExpr ~depth:(depth+1) condExpr;
			printStmt ~depth:(depth+1) thenStmt;
			(match elseStmt with
				None -> ()
				|Some stmt -> printStmt ~depth:(depth+1) stmt)
		)
	|ITERATION_STMT (expr, stmt) -> 
		printForm ~depth ~tag:"ITERATION_STMT" ~printee:
		(fun _ -> 
			printExpr ~depth:(depth+1) expr;
			printStmt ~depth:(depth+1) stmt
		)
	|RETURN_STMT (exprOption)->
		printForm ~depth ~tag:"RETRUN_STMT" ~printee:
		(fun _ -> 
			match exprOption with
				None -> ()
				|Some expr -> printExpr ~depth:(depth+1) expr
		)
and printExpr ~depth = function
	ASSIGN_EXPRESSION (var, expr) ->
		printForm ~depth ~tag:"ASSIGN_EXPRESSION" ~printee:
		(fun _ -> 
			print ~depth:(depth+1) "name: %s\n" var;
			printExpr ~depth:(depth+1) expr
		)
	|ARRAY_ASSIGN_EXPRESSION (var, indexExpr, expr) -> 
		printForm ~depth ~tag:"ARRAY_ASSIGN_EXPRESSION" ~printee:
		(fun _ -> 
			print ~depth:(depth+1) "name: %s\n" var;
			printExpr ~depth:(depth+1) indexExpr;
			printExpr ~depth:(depth+1) expr		
		)
	|ARITHMATIC_EXPRESSION (aexp) -> printAExpr ~depth aexp
	|BOOLEAN_EXPRESSION (bexp) -> printBExpr ~depth bexp
and printExprForm ~depth ~tag ~aexp1 ~aexp2 = 
		printForm ~depth ~tag ~printee:
		(fun _ -> 
			printAExpr ~depth:(depth+1) aexp1;
			printAExpr ~depth:(depth+1) aexp2
		)
and printAExpr ~depth = function
	PLUS 		(aexp1, aexp2) -> printExprForm ~depth ~tag:"PLUS" ~aexp1 ~aexp2
	|MINUS 		(aexp1, aexp2) -> printExprForm ~depth ~tag:"MINUS" ~aexp1 ~aexp2
	|MULTIPLY	(aexp1, aexp2) -> printExprForm ~depth ~tag:"MULTIPLY" ~aexp1 ~aexp2
	|DIV		(aexp1, aexp2) -> printExprForm ~depth ~tag:"DIV" ~aexp1 ~aexp2
	|NUM value -> print ~depth "number: %d\n" value
	|VAR name -> print ~depth "name: %s\n" name
	|ARRAY_VAR (name, expr) -> 
		printForm ~depth ~tag:"ARRAY_VAR" ~printee:
		(fun _ -> 
			print ~depth:(depth+1) "name: %s\n" name;
			printExpr ~depth:(depth+1) expr
		)
	|EXPRESSION expr -> printExpr ~depth:(depth+1) expr
	|CALL (name, exprs) -> 
		printForm ~depth ~tag:"CALL" ~printee:
		(fun _ -> 
			print ~depth:(depth+1) "name: %s\n" name;
			printList ~depth:(depth+1) ~printer:printExpr exprs
		)
and printBExpr ~depth = function
	LESSEQUAL		(aexp1, aexp2) -> printExprForm ~depth ~tag:"LESSEQUAL" ~aexp1 ~aexp2
	|LESS			(aexp1, aexp2) -> printExprForm ~depth ~tag:"LESS" ~aexp1 ~aexp2
	|GREATER		(aexp1, aexp2) -> printExprForm ~depth ~tag:"GREATER" ~aexp1 ~aexp2
	|GREATEREQUAL	(aexp1, aexp2) -> printExprForm ~depth ~tag:"GREATEREQUAL" ~aexp1 ~aexp2
	|EQUAL			(aexp1, aexp2) -> printExprForm ~depth ~tag:"EQUAL" ~aexp1 ~aexp2
	|NOTEQUAL		(aexp1, aexp2) -> printExprForm ~depth ~tag:"NOTEQUAL" ~aexp1 ~aexp2