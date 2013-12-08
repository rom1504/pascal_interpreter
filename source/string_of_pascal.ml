(** Convert program to a string which contain a program *)

open Type;;

(** Convert a pascal type to a string *)
let rec string_of_pascal_type pascal_type=match pascal_type	with
	| Integer -> "integer"
	| Boolean -> "boolean"
	| Array sub_pascal_type -> "array of "^(string_of_pascal_type sub_pascal_type);;

(** Convert an environment to a string *)
let string_of_environment env = String.concat "; " (List.map (fun (identifier_list,pascal_type) -> (String.concat "," identifier_list)^" : "^(string_of_pascal_type pascal_type)) env);;

(** Convert an optional variable definition to a string *)
let string_of_optional_variable_definition env = if env=[] then "" else "var "^(string_of_environment env)^";\n";;
	
(** Convert a constant to a string *)
let string_of_constant constant=match constant with
	| Int n -> string_of_int n
	| Bool b -> if b then "true" else "false";;
	
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
	| Equal -> "="
	| Diff -> "<>";;
	
(** Convert an expression to a string *)
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
		
(** Convert a list of expression to a string *)
and string_of_expression_list expression_list=String.concat "," (List.map string_of_expression expression_list);;

(** Convert a condition to a string *)
let rec string_of_condition condition=match condition with
	| Expr expression -> string_of_expression expression
	| Negation condition -> "not "^(string_of_condition condition)
	| Or (condition1,condition2) -> (string_of_condition condition1)^" or "^(string_of_condition condition2)
	| And (condition1,condition2) -> (string_of_condition condition1)^" and "^(string_of_condition condition2)
	| Bracket condition -> "("^(string_of_condition condition)^")";;
	
(** Convert an instruction to a string *)
let rec string_of_instruction instruction=match instruction with
	| Proc_call (identifier,expression_list) -> identifier^"("^(string_of_expression_list expression_list)^")"
	| Var_assign (identifier,expression) -> identifier^":="^(string_of_expression expression)
	| Array_assign (expression1,expression2,expression3) -> (string_of_expression expression1)^"["^(string_of_expression expression2)^"]:="^(string_of_expression expression3)
	| Conditional (condition,instruction1,instruction2) -> "if "^(string_of_condition condition)^" then\n"^(string_of_instruction instruction1)^"\nelse "^(string_of_instruction instruction2)
	| Loop (condition,instruction) -> "while "^(string_of_condition condition)^" do\n"^(string_of_instruction instruction)
	| Block instruction_list -> "begin\n"^(string_of_instruction_list instruction_list)^"end"
	
(** Convert an instruction list to a string *)
and string_of_instruction_list instr_list=String.concat ";\n" (List.map string_of_instruction instr_list);;

(** Convert a procedure definition to a string *)
let string_of_proc_def (identifier,environment,variable_definition,instruction_list)=
	"procedure "^identifier^"("^(string_of_environment environment)^") ;\n"
	^(string_of_optional_variable_definition variable_definition)
	^"begin\n"
	^(string_of_instruction_list instruction_list)^"\n"^
	"end;";;

(** Convert a function definition to a string *)
let string_of_func_def (identifier,environment,return_type,variable_definition,instruction_list)=
	"function "^identifier^"("^(string_of_environment environment)^") : "^(string_of_pascal_type return_type)^";\n"
	^(string_of_optional_variable_definition variable_definition)
	^"begin\n"
	^(string_of_instruction_list instruction_list)^"\n"^
	"end;";;

(** Convert a procedure or function definition to a string *)
let string_of_proc_or_func_def proc_func=match proc_func with | Func f -> string_of_func_def f | Proc p -> string_of_proc_def p;;

(** Convert a procedure or function definition list to a string *)
let string_of_proc_func_list proc_func_list=(String.concat "\n\n" (List.map string_of_proc_or_func_def proc_func_list))^(if proc_func_list=[] then "" else "\n\n");;

(** Convert a program to a string *)
let string_of_program (env,proc_func_list,instr_list) = "program\n"^(string_of_optional_variable_definition env)^(string_of_proc_func_list proc_func_list)^"begin\n"^(string_of_instruction_list instr_list)^"\nend.";;


(** Print a pascal program converted to a string *)
let pretty_print a=print_string (string_of_program a);;