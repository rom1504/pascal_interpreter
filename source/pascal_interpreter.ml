(** Pascal interpreter *)

open Type;;
open Parser;;
open String_of_pascal;;

module StringMap = Map.Make (String);;

type pascal_value=
	VInt of int
	| VBool of bool
 	| VArray of pascal_value array
	| Undefined;;
	
type small_valued_environment=(Type.pascal_type*pascal_value) StringMap.t;;



type valued_environment=small_valued_environment*small_valued_environment;;

type function_map=Type.pascal_function StringMap.t;;

type procedure_map=Type.procedure StringMap.t;;
		
type pratical_environment=valued_environment*procedure_map*function_map;;

exception FunctionNotDefined;;
exception ProcedureNotDefined of string;;
exception MissingParameter;;
exception TooMuchParameters;;
exception TypeError of string;;
exception VariableNotDeclared of string;;
exception DivideByZero;;
exception IndexUnbound of int;;
exception MissingReturnInFunction;;
exception NotDoneYet;;
exception NotAnIdentifier;;
exception Problem;;

let default_value_for_type pascal_type=match pascal_type with 
		| Boolean -> VBool false
		| Integer -> VInt 0
		| _ -> Undefined;;

let rec string_of_pascal_value=function
	| VInt i -> "VInt("^(string_of_int i)^")"
	| VBool b -> "VBool("^(if b then "true" else "false")^")"
	| VArray pascal_value_array -> "VArray("^(String.concat "," (Array.fold_left (fun string_list pascal_value -> (string_of_pascal_value pascal_value)::string_list) [] pascal_value_array))^")"
	| Undefined -> "Undefined"
	


let string_of_small_valued_environment small_valued_environment=String.concat ", " (StringMap.fold (fun identifier (pascal_type,pascal_value) l -> (identifier^":"^(String_of_pascal.string_of_pascal_type pascal_type)^":"^(string_of_pascal_value pascal_value))::l) small_valued_environment []);;

let rec valued_environment_of_environment environment =
	List.fold_left (fun valued_environment (identifier_list,pascal_type) ->
		(List.fold_left (fun valued_environment identifier -> StringMap.add identifier (pascal_type,(default_value_for_type pascal_type)) valued_environment) valued_environment identifier_list)) StringMap.empty environment;;

let decompose_procedure_or_function_list procedure_or_function_list=List.fold_left
	(fun (procedure_map,function_map) procedure_or_function -> (match procedure_or_function with
		| Proc ((identifier,_,_,_) as p) -> (StringMap.add identifier p procedure_map,function_map)
		| Func ((identifier,_,_,_,_) as f) -> (procedure_map,StringMap.add identifier f function_map)
	)) (StringMap.empty,StringMap.empty) procedure_or_function_list;;

let pratical_environment_of_environment_and_procedure_or_function_list environment procedure_or_function_list=
	let valued_environement=(valued_environment_of_environment environment,StringMap.empty) in
	let (procedure_map,function_map)=decompose_procedure_or_function_list procedure_or_function_list in
	(valued_environement,procedure_map,function_map);;

	
let coherent_value_type pascal_value1 pascal_value2 =
	let rec check_coherent_value_type pascal_value1 pascal_value2 = match (pascal_value1,pascal_value2) with
		| (VInt _,VInt _) | (VBool _,VBool _) | (Undefined,_) |  (_,Undefined) -> true
		| (VArray pascal_value_array1,VArray pascal_value_array2) -> check_coherent_value_type (Array.get pascal_value_array1 0) (Array.get pascal_value_array2 0)
		| _ -> false
	in if check_coherent_value_type pascal_value1 pascal_value2 then () else raise (TypeError ((string_of_pascal_value pascal_value1)^" type is not coherent with the type of "^(string_of_pascal_value pascal_value2)));;
	
let check_type pascal_value pascal_type identifier =
	let rec correct_type pascal_value pascal_type = match (pascal_value,pascal_type) with
		| (VInt _,Integer) | (VBool _,Boolean) | (Undefined,_) -> true
		| (VArray pascal_value_array,Array pascal_type) -> Array.fold_left (fun is_correct pascal_value -> is_correct && (correct_type pascal_value pascal_type))  true pascal_value_array
		| _ -> false
	in if correct_type pascal_value pascal_type then () else raise (TypeError ("the variable "^identifier^" of type "^(String_of_pascal.string_of_pascal_type pascal_type)^" cannot be assigned the value  "^(string_of_pascal_value pascal_value)));;

let defined_valued_environment_of_environment_and_value_list environment value_list =
	let value_list=List.rev value_list in 
	let (value_list,valued_environment) = (List.fold_left (fun (value_list,valued_environment) (identifier_list,pascal_type) ->
		(List.fold_left (fun (value_list,valued_environment) identifier ->
		(match value_list with
			| value::value_list -> let _ = check_type value pascal_type identifier in (value_list,StringMap.add identifier (pascal_type,value) valued_environment)
			| [] -> raise MissingParameter
		)) (value_list,valued_environment) identifier_list)) (value_list,StringMap.empty) environment) in
	if value_list=[] then valued_environment else raise TooMuchParameters;;


let execute_constant=function
	| Int i -> VInt i
	| Bool b -> VBool b;;
	
let execute_variable_access (global_valued_environment,local_valued_environment) identifier=
	let (_,value) = 
		(try (StringMap.find identifier local_valued_environment) with Not_found ->
			(try (StringMap.find identifier global_valued_environment) with Not_found ->
			raise (VariableNotDeclared identifier)))
		in (global_valued_environment,value);;

let rec execute_array_creation pratical_environment pascal_type expression=
	let (global_valued_environment,number)=int_of_expression pratical_environment expression in
	(global_valued_environment,VArray (Array.make number (default_value_for_type pascal_type)))
	
and execute_array_access (((global_valued_environment,local_valued_environment),procedure_map,function_map) as pratical_environment) expression1 expression2=
	let (global_valued_environment,varray)=array_of_expression pratical_environment expression1 in
	let (global_valued_environment,index)=int_of_expression ((global_valued_environment,local_valued_environment),procedure_map,function_map) expression2 in 
	(global_valued_environment,try Array.get varray index with  Invalid_argument "index out of bounds" -> raise (IndexUnbound index))
	
		
and execute_readln expression_list =
	if expression_list != [] then raise TooMuchParameters else
	VInt (read_int())
	
and execute_function_call (((global_valued_environment,_),procedure_map,function_map) as pratical_environment) identifier expression_list=
	if identifier = "readln" then (global_valued_environment,execute_readln expression_list)
	else
	let (identifier,parameter_environment,return_type,local_environment,instruction_list)=(try StringMap.find identifier function_map with Not_found -> raise FunctionNotDefined) in
	let (global_valued_environment,value_list)=execute_expression_list pratical_environment expression_list in
	let valued_parameter_environment=defined_valued_environment_of_environment_and_value_list parameter_environment value_list in
	let valued_local_environment=valued_environment_of_environment local_environment in
	let local_valued_environment=merge_valued_environment valued_parameter_environment valued_local_environment in
	let local_valued_environment=StringMap.add identifier (return_type,(default_value_for_type return_type)) local_valued_environment in
	let (global_valued_environment,local_valued_environment)=execute_instruction_list ((global_valued_environment,local_valued_environment),procedure_map,function_map) instruction_list in
	let return_value = try let (_,return_value) = StringMap.find identifier local_valued_environment in return_value with Not_found -> raise MissingReturnInFunction in
 	(global_valued_environment,return_value)
	
and int_of_expression pratical_environment expression=
	let (global_valued_environment,value)=execute_expression pratical_environment expression in
	(global_valued_environment,match value with | VInt i -> i | _ -> raise (TypeError ((string_of_pascal_value value)^" is not a VInt")))
and array_of_expression pratical_environment expression=
	let (global_valued_environment,value)=execute_expression pratical_environment expression in
	(global_valued_environment,match value with | VArray a -> a | _ -> raise (TypeError ((string_of_pascal_value value)^" is not a VArray")))

and execute_unary_minus pratical_environment expression=
	let (global_valued_environment,n)=int_of_expression pratical_environment expression in (global_valued_environment,VInt(-n))

and execute_arithmetic_operation (((global_valued_environment,local_valued_environment),procedure_map,function_map) as pratical_environment) expression1 op expression2=
	let (global_valued_environment,a)=int_of_expression pratical_environment expression1 in
	let (global_valued_environment,b)=int_of_expression ((global_valued_environment,local_valued_environment),procedure_map,function_map) expression2 in
	(global_valued_environment,VInt(match op with
	| Plus -> a+b
	| Mult -> a*b
	| Div -> if b=0 then raise DivideByZero else a/b
	| Minus -> a-b))
	
and execute_comparison (((global_valued_environment,local_valued_environment),procedure_map,function_map) as pratical_environment) expression1 comp expression2=
	let (global_valued_environment,a)=int_of_expression pratical_environment expression1 in
	let (global_valued_environment,b)=int_of_expression ((global_valued_environment,local_valued_environment),procedure_map,function_map) expression2 in
	(global_valued_environment,VBool(match comp with
	| Inf -> a<b
	| Inf_equal -> a<=b
	| Sup -> a>b
	| Sup_equal -> a>=b
	| Equal -> a=b
	| Diff -> a!=b))

and execute_expression ((((global_valued_environment,local_valued_environment) as valued_environment,procedure_map,function_map) as pratical_environment):pratical_environment) expression=match expression with
	| Const c -> (global_valued_environment,execute_constant c)
	| Var identifier -> execute_variable_access valued_environment identifier
	| Unary_minus expression -> execute_unary_minus pratical_environment expression
	| BracketE expression -> execute_expression pratical_environment expression
	| Arithmetic_operation (expression1,op,expression2) -> execute_arithmetic_operation pratical_environment expression1 op expression2
	| Comparison (expression1,comp,expression2) -> execute_comparison pratical_environment expression1 comp expression2
	| Function_call (identifier,expression_list) -> execute_function_call pratical_environment identifier expression_list
	| Array_access (expression1,expression2) -> execute_array_access pratical_environment expression1 expression2
	| Array_creation (pascal_type,expression) -> execute_array_creation pratical_environment pascal_type expression
	
(* 	pb là ? oui : liste ici et dans defined_valued... pas dans le même sens *)
and execute_expression_list ((global_valued_environment,local_valued_environment),procedure_map,function_map) expression_list= List.fold_left (fun (global_valued_environment,value_list) expression-> let (global_valued_environment,value)=execute_expression ((global_valued_environment,local_valued_environment),procedure_map,function_map) expression in (global_valued_environment,value::value_list)) (global_valued_environment,[]) expression_list

and execute_assign ((((global_valued_environment,local_valued_environment),_,_) as pratical_environment):pratical_environment) identifier expression=
	let (global_valued_environment,value)=execute_expression pratical_environment expression in
	let (local,(pascal_type,_)) = 
		(try (true,StringMap.find identifier local_valued_environment) with Not_found ->
			(try (false,StringMap.find identifier global_valued_environment) with Not_found ->
			raise (VariableNotDeclared identifier)))
		in 
	let _ = check_type value pascal_type identifier in 
	if local
	then 
	(global_valued_environment,StringMap.add identifier (pascal_type,value) local_valued_environment)
	else
	(StringMap.add identifier (pascal_type,value) global_valued_environment,local_valued_environment)

and execute_array_assign (((global_valued_environment,local_valued_environment),procedure_map,function_map) as pratical_environment) expression1 expression2 expression3=
	let (global_valued_environment,varray)=array_of_expression pratical_environment expression1 in
	let (global_valued_environment,index)=int_of_expression ((global_valued_environment,local_valued_environment),procedure_map,function_map) expression2 in
	let (global_valued_environment,value)=execute_expression ((global_valued_environment,local_valued_environment),procedure_map,function_map) expression3 in
 	let _ = coherent_value_type (Array.get varray 0) value in
	let _=try Array.set varray index value  with  Invalid_argument "index out of bounds" -> raise (IndexUnbound index) in
	(global_valued_environment,local_valued_environment)

	
and execute_loop (((global_valued_environment,local_valued_environment),procedure_map,function_map) as pratical_environment) condition instruction = 
	let (global_valued_environment,result) = execute_condition pratical_environment condition in
	if result
	then execute_loop ((execute_instruction ((global_valued_environment,local_valued_environment),procedure_map,function_map) instruction),procedure_map,function_map) condition instruction
	else (global_valued_environment,local_valued_environment)
	
and execute_condition (((global_valued_environment,local_valued_environment),procedure_map,function_map) as pratical_environment) condition=match condition with
	| Expr expression ->
		(
		let (global_valued_environment,value)=execute_expression pratical_environment expression in
			match value with
		| VBool b -> (global_valued_environment,b)
		| VInt i -> (global_valued_environment,i!=0)
		| _ -> raise (TypeError "is not bool or int"))
	| Negation condition -> let (global_valued_environment,result)=execute_condition pratical_environment condition in (global_valued_environment, not result)
	| Or (condition1,condition2) ->
		let (global_valued_environment,result1) = execute_condition pratical_environment condition1 in 
		if result1 then (global_valued_environment,true) else
		let (global_valued_environment,result2) = execute_condition ((global_valued_environment,local_valued_environment),procedure_map,function_map) condition2 in
		(global_valued_environment,result2)
	| And (condition1,condition2) -> 
		let (global_valued_environment,result1) = execute_condition pratical_environment condition1 in 
		if not result1 then (global_valued_environment,false) else
		let (global_valued_environment,result2) = execute_condition ((global_valued_environment,local_valued_environment),procedure_map,function_map) condition2 in
		(global_valued_environment,result2)
	| Bracket condition -> execute_condition pratical_environment condition
	
and execute_conditional (((global_valued_environment,local_valued_environment),procedure_map,function_map) as pratical_environment) condition instruction1 instruction2=
	let (global_valued_environment,result) = execute_condition pratical_environment condition in
	if result
	then execute_instruction ((global_valued_environment,local_valued_environment),procedure_map,function_map) instruction1
	else execute_instruction ((global_valued_environment,local_valued_environment),procedure_map,function_map) instruction2
		
and execute_writeln (((global_valued_environment,previous_local_valued_environment),_,_) as pratical_environment) expression_list =
	let expression = match expression_list with
	| expression::[] -> expression
	| _ -> raise TooMuchParameters in
	let (global_valued_environment,number)=int_of_expression pratical_environment expression in
	let _ = print_int number in
	let _ = print_newline() in
	(global_valued_environment,previous_local_valued_environment)
	
and merge_valued_environment high_priority_environment low_priority_environment=StringMap.merge (fun identifier high_priority_value low_priority_value -> 
		(match (high_priority_value,low_priority_value) with 
			| (None,None) -> None
			| (Some value,None) | (None,Some value) -> Some value
			| (Some high_priority_value,Some low_priority_value) -> Some high_priority_value)) high_priority_environment low_priority_environment
	
and execute_proc_call (((global_valued_environment,previous_local_valued_environment),procedure_map,function_map) as pratical_environment) identifier expression_list=
	if identifier = "writeln" || identifier = "write" then execute_writeln pratical_environment expression_list
	else
	let (identifier,parameter_environment,local_environment,instruction_list)=(try StringMap.find identifier procedure_map with Not_found -> raise (ProcedureNotDefined identifier)) in
	let (global_valued_environment,value_list)=execute_expression_list pratical_environment expression_list in
	let valued_parameter_environment=defined_valued_environment_of_environment_and_value_list parameter_environment value_list in
	let valued_local_environment=valued_environment_of_environment local_environment in
	let local_valued_environment=merge_valued_environment valued_parameter_environment valued_local_environment in
	let (global_valued_environment,_) = execute_instruction_list((global_valued_environment,local_valued_environment),procedure_map,function_map) instruction_list in
	(global_valued_environment,previous_local_valued_environment)
	
and execute_instruction (pratical_environment:pratical_environment) instruction=match instruction with
	| Proc_call (identifier,expression_list) -> execute_proc_call pratical_environment identifier expression_list
	| Var_assign (identifier,expression) -> execute_assign pratical_environment identifier expression
	| Array_assign (expression1,expression2,expression3) -> execute_array_assign pratical_environment expression1 expression2 expression3
	| Conditional (condition,instruction1,instruction2) -> execute_conditional pratical_environment condition instruction1 instruction2
	| Loop (condition,instruction) -> execute_loop pratical_environment condition instruction
	| Block (instruction_list) -> execute_instruction_list pratical_environment instruction_list
	

and execute_instruction_list (valued_environment,procedure_map,function_map) instruction_list=
	List.fold_left (fun ((global_valued_environment,local_valued_environment) as valued_environment) instruction ->
		execute_instruction (valued_environment,procedure_map,function_map) instruction) valued_environment instruction_list;;


let execute_program (environment,procedure_or_function_list,instruction_list)=
	execute_instruction_list (pratical_environment_of_environment_and_procedure_or_function_list environment procedure_or_function_list) instruction_list;;
	
let filename=Sys.argv.(1);;
execute_program (Parser.main Lexer.token (Lexing.from_channel (open_in filename)));;