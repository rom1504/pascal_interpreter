open Type;;
open Parser;;
open Make_call_graph;;


let string_of_call_graph call_graph=
	"digraph call_graph {\n  program;\n"^
	(IdentifierMap.fold 
	(fun identifier1 identifier_set s1 -> 
		IdentifierSet.fold (fun (identifier2,terminal) s2 -> s2^"  "^identifier1^" -> "^identifier2^(if terminal then " [ style = dashed ]" else "")^";\n") identifier_set s1)
	call_graph "")
	^"}";;

let pretty_print a=print_string(string_of_call_graph(a));;

pretty_print(Make_call_graph.make_call_graph(Parser.main Lexer.token (Lexing.from_channel stdin)));;
print_newline();;
