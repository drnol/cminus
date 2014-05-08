open Pretty_printer

let result = ref None

let _ = 
	if Array.length Sys.argv > 1 then
	let filein = open_in Sys.argv.(1) in
	let lexbuf = Lexing.from_channel filein in
		result := Some (Cminusparser.program Cminuslexer.cminustoken lexbuf);
		flush stdout;
	close_in filein
	
let _ = 
	print_string "\n*** cminus printing ***\n\n";
	match !result with
	None -> print_string "no program...\n"
	|Some program -> printAST program
		



