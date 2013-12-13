(** Convert program to a string which contain a program in c *)

open Type;;
open Parser;;


let clean_string_of_func_or_proc identifier=
	if identifier = "pow" || identifier = "malloc" || identifier = "double"
	then "my_"^identifier
	else identifier;;

(** Convert a pascal type to a string *)
let rec string_of_pascal_type pascal_type=match pascal_type	with
	| Integer -> "int"
	| Boolean -> "int"
	| Array sub_pascal_type -> (string_of_pascal_type sub_pascal_type)^"*";;

let rec stars_of_pascal_type pascal_type=match pascal_type	with
	| Integer -> ""
	| Boolean -> ""
	| Array sub_pascal_type -> (stars_of_pascal_type sub_pascal_type)^"*";;
	
(** Convert an environment to a string *)
let string_of_environment tab env = 
	String.concat "" (List.map (fun (identifier_list,pascal_type) ->
	let stars=stars_of_pascal_type pascal_type in
	tab^"int "
	^(String.concat "," (List.map (fun identifier -> stars^identifier^(match pascal_type with
		| Integer | Boolean -> "=0"
		| _ -> ""
	)) identifier_list))^";\n") env);;

(** Convert an environment to a string in parameter *)
let string_of_environment_in_parameter env = String.concat "," (List.map (fun (identifier_list,pascal_type) -> (String.concat "," (List.map (fun identifier -> (string_of_pascal_type pascal_type)^" "^identifier) identifier_list))) env);;

(** Convert an optional variable definition to a string *)
let string_of_optional_variable_definition tab env = if env=[] then "" else (string_of_environment tab env);;
	
(** Convert a constant to a string *)
let string_of_constant constant=match constant with
	| Int n -> string_of_int n
	| Bool b -> if b then "1" else "0";;
	
(** Convert an operation to a string *)
let string_of_op op=match op with
	| Plus -> "+"
	| Minus -> "-"
	| Mult -> "*"
	| Div -> "/";;
	
(** Convert a comparison operand to a string *)
let string_of_comp comp=match comp with
	| Inf -> "<"
	| Inf_equal -> "<="
	| Sup -> ">"
	| Sup_equal -> ">="
	| Equal -> "=="
	| Diff -> "!=";;
	
(** Convert an expression to a string *)
let rec string_of_expression in_function_definition_identifier expression=match expression with
	| Const constant -> string_of_constant constant
	| Var identifier -> (if in_function_definition_identifier != "" && in_function_definition_identifier=identifier then "__"^(clean_string_of_func_or_proc identifier)^"__" else identifier)
	| Unary_minus expression -> "(-"^(string_of_expression in_function_definition_identifier expression)^")"
	| BracketE expression -> "("^(string_of_expression in_function_definition_identifier expression)^")"
	| Arithmetic_operation (expression1,op,expression2) ->
		(string_of_expression in_function_definition_identifier expression1)^(string_of_op op)^(string_of_expression in_function_definition_identifier (if (expression2 = Const (Int 0) && op=Div) then (Const (Int 1)) else expression2))
	| Comparison (expression1,comp,expression2) -> (string_of_expression in_function_definition_identifier expression1)^(string_of_comp comp)^(string_of_expression in_function_definition_identifier expression2)
	| Function_call (identifier,expression_list) -> (clean_string_of_func_or_proc identifier)^"("^(string_of_expression_list in_function_definition_identifier expression_list)^")"
	| Array_access (expression1,expression2) -> (string_of_expression in_function_definition_identifier expression1)^"["^(string_of_expression in_function_definition_identifier expression2)^"]"
	| Array_creation (pascal_type,expression) -> "malloc(sizeof("^(string_of_pascal_type pascal_type)^")*("^(string_of_expression in_function_definition_identifier expression)^"))"
		
(** Convert a list of expression to a string *)
and string_of_expression_list in_function_definition_identifier expression_list=String.concat "," (List.map (string_of_expression in_function_definition_identifier) expression_list);;

(** Convert a condition to a string *)
let rec string_of_condition in_function_definition_identifier condition=match condition with
	| Expr expression -> string_of_expression in_function_definition_identifier expression
	| Negation condition -> "!"^(string_of_condition in_function_definition_identifier condition)
	| Or (condition1,condition2) -> (string_of_condition in_function_definition_identifier condition1)^" || "^(string_of_condition in_function_definition_identifier condition2)
	| And (condition1,condition2) -> (string_of_condition in_function_definition_identifier condition1)^" && "^(string_of_condition in_function_definition_identifier condition2)
	| Bracket condition -> "("^(string_of_condition in_function_definition_identifier condition)^")";;
	
(** Convert an instruction to a string *)
let rec string_of_instruction btab tab in_function_definition_identifier instruction=match instruction with
	| Proc_call (identifier,expression_list) -> (if btab then tab else "")^(clean_string_of_func_or_proc identifier)^"("^(string_of_expression_list in_function_definition_identifier expression_list)^");\n"
	| Var_assign (identifier,expression) -> (if btab then tab else "")^(if in_function_definition_identifier != "" && in_function_definition_identifier=identifier then "__"^(clean_string_of_func_or_proc identifier)^"__" else identifier)^"="^(string_of_expression in_function_definition_identifier expression)^";\n"
	| Array_assign (expression1,expression2,expression3) -> (if btab then tab else "")^(string_of_expression in_function_definition_identifier expression1)^"["^(string_of_expression in_function_definition_identifier expression2)^"]="^(string_of_expression in_function_definition_identifier expression3)^";\n"
	| Conditional (condition,instruction1,instruction2) -> (if btab then tab else "")^"if("^(string_of_condition in_function_definition_identifier condition)^") "^(string_of_instruction false tab in_function_definition_identifier instruction1)^tab^"else "^(string_of_instruction false tab in_function_definition_identifier instruction2)
	| Loop (condition,instruction) -> (if btab then tab else "")^"while("^(string_of_condition in_function_definition_identifier condition)^") "^(string_of_instruction false tab in_function_definition_identifier instruction)
	| Block instruction_list -> "\n"^tab^"{\n"^(string_of_instruction_list (tab^"\t") in_function_definition_identifier instruction_list)^tab^"}\n"
	
(** Convert an instruction list to a string *)
and string_of_instruction_list tab in_function_definition_identifier instr_list=(String.concat "" (List.map (string_of_instruction true tab in_function_definition_identifier) instr_list));;

(** Convert a procedure definition to a string *)
let string_of_proc_def (identifier,environment,variable_definition,instruction_list)=
	"void "^(clean_string_of_func_or_proc identifier)^"("^(string_of_environment_in_parameter environment)^")\n{\n"
	^(string_of_optional_variable_definition "\t" variable_definition)
	^(string_of_instruction_list "\t" "" instruction_list)^
	"}";;

(** Convert a function definition to a string *)
let string_of_func_def (identifier,environment,return_type,variable_definition,instruction_list)=
	let nidentifier=clean_string_of_func_or_proc identifier in
	(string_of_pascal_type return_type)^" "^nidentifier^"("^(string_of_environment_in_parameter environment)^")\n{\n"
	^(string_of_optional_variable_definition "\t" ((((" __"^nidentifier^"__")::[]),return_type)::variable_definition))
	^(string_of_instruction_list "\t" identifier instruction_list)^
	"\treturn __"^nidentifier^"__;\n"^
	"}";;

(** Convert a procedure or function definition to a string *)
let string_of_proc_or_func_def proc_func=match proc_func with | Func f -> string_of_func_def f | Proc p -> string_of_proc_def p;;

(** Convert a procedure or function definition list to a string *)
let string_of_proc_func_list proc_func_list=(String.concat "\n\n" (List.map string_of_proc_or_func_def proc_func_list))^(if proc_func_list=[] then "" else "\n\n");;

(** Convert a program to a string *)
let string_of_program (env,proc_func_list,instr_list) = "#include <stdio.h>\n#include <stdlib.h>\n\nvoid writeln(int a){printf(\"%d\\n\",a);} \nvoid write(int a){printf(\"%d\\n\",a);} \nint readln(){int a;scanf(\"%d\",&a); return a;}\n\n"^(string_of_optional_variable_definition "" env)^(string_of_proc_func_list proc_func_list)^"int main()\n{\n"^(string_of_instruction_list "\t" "" instr_list)^"\treturn 0;\n}";;


(** Print a pascal program converted to a string *)
let pretty_print a=print_string (string_of_program a);;

pretty_print (Parser.main Lexer.token (Lexing.from_channel stdin));;
print_newline();;