(** Convert program to a string which contain a program *)

open Type;;
open Parser;;

let rec string_of_pascal_type pascal_type=match pascal_type	with
	| Integer -> "integer"
	| Boolean -> "boolean"
	| Array sub_pascal_type -> "array of "^(string_of_pascal_type sub_pascal_type);;

let string_of_environment env = String.concat "; " (List.map (fun (identifier_list,pascal_type) -> (String.concat "," identifier_list)^" : "^(string_of_pascal_type pascal_type)) env);;

let string_of_optional_variable_definition env = if env=[] then "" else "var "^(string_of_environment env)^";\n";;
	
let string_of_constant constant=match constant with
	| Int n -> string_of_int n
	| Bool b -> if b then "true" else "false";;
	
let string_of_op op=match op with
	| Plus -> "+"
	| Minus -> "-"
	| Mult -> "*"
	| Div -> "/";;
	
let string_of_comp comp=match comp with
	| Inf -> "<"
	| Inf_equal -> "<="
	| Sup -> ">"
	| Sup_equal -> ">="
	| Equal -> "="
	| Diff -> "<>";;
	
let rec string_of_expression expression=match expression with
	| Const constant -> string_of_constant constant
	| Var identifier -> identifier
	| Unary_minus expression -> "-"^(string_of_expression expression)
	| BracketE expression -> "("^(string_of_expression expression)^")"
	| Arithmetic_operation (expression1,op,expression2) -> (string_of_expression expression1)^(string_of_op op)^(string_of_expression expression2)
	| Comparison (expression1,comp,expression2) -> (string_of_expression expression1)^(string_of_comp comp)^(string_of_expression expression2)
	| Function_call (identifier,expression_list) -> identifier^"("^(string_of_expression_list expression_list)^")"
	| Array_access (expression1,expression2) -> (string_of_expression expression1)^"["^(string_of_expression expression2)^"]"
	| Array_creation (pascal_type,expression) -> "new array of "^(string_of_pascal_type pascal_type)^"["^(string_of_expression expression)^"]"
		
and string_of_expression_list expression_list=String.concat "," (List.map string_of_expression expression_list);;

let rec string_of_condition condition=match condition with
	| Expr expression -> string_of_expression expression
	| Negation condition -> "not "^(string_of_condition condition)
	| Or (condition1,condition2) -> (string_of_condition condition1)^" or "^(string_of_condition condition2)
	| And (condition1,condition2) -> (string_of_condition condition1)^" and "^(string_of_condition condition2)
	| Bracket condition -> "("^(string_of_condition condition)^")";;
	
let rec string_of_instruction instruction=match instruction with
	| Proc_call (identifier,expression_list) -> identifier^"("^(string_of_expression_list expression_list)^")"
	| Var_assign (identifier,expression) -> identifier^":="^(string_of_expression expression)
	| Array_assign (expression1,expression2,expression3) -> (string_of_expression expression1)^"["^(string_of_expression expression2)^"]:="^(string_of_expression expression3)
	| Conditional (condition,instruction1,instruction2) -> "if "^(string_of_condition condition)^" then\n"^(string_of_instruction instruction1)^"\nelse "^(string_of_instruction instruction2)
	| Loop (condition,instruction) -> "while "^(string_of_condition condition)^" do\n"^(string_of_instruction instruction)
	| Block instruction_list -> "begin\n"^(string_of_instruction_list instruction_list)^"end"
	
and string_of_instruction_list instr_list=String.concat ";\n" (List.map string_of_instruction instr_list);;

let string_of_proc_def (identifier,environment,variable_definition,instruction_list)=
	"procedure "^identifier^"("^(string_of_environment environment)^") ;\n"
	^(string_of_optional_variable_definition variable_definition)
	^"begin\n"
	^(string_of_instruction_list instruction_list)^"\n"^
	"end;";;

let string_of_func_def (identifier,environment,return_type,variable_definition,instruction_list)=
	"function "^identifier^"("^(string_of_environment environment)^") : "^(string_of_pascal_type return_type)^";\n"
	^(string_of_optional_variable_definition variable_definition)
	^"begin\n"
	^(string_of_instruction_list instruction_list)^"\n"^
	"end;";;

let string_of_proc_or_func_def proc_func=match proc_func with | Func f -> string_of_func_def f | Proc p -> string_of_proc_def p;;

let string_of_proc_func_list proc_func_list=(String.concat "\n\n" (List.map string_of_proc_or_func_def proc_func_list))^(if proc_func_list=[] then "" else "\n\n");;

let string_of_program (env,proc_func_list,instr_list) = "program\n"^(string_of_optional_variable_definition env)^(string_of_proc_func_list proc_func_list)^"begin\n"^(string_of_instruction_list instr_list)^"\nend.";;


let pretty_print a=print_string (string_of_program a);;

pretty_print (Parser.main Lexer.token (Lexing.from_channel stdin));;
print_newline();;