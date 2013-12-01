open Type;;

module IdentifierMap = Map.Make (String);;
module IdentifierSet = Set.Make (struct
    type t = string*bool
    let compare = Pervasives.compare
  end);;

type call_graph=IdentifierSet.t IdentifierMap.t;;
    
let link terminal f1 f2 map=
	let ens=try IdentifierMap.find f1 map with Not_found -> IdentifierSet.empty in
	IdentifierMap.add f1 (IdentifierSet.add (f2,terminal) ens) map;;

let rec make_call_graph_expression terminal prev_identifier map expression=match expression with
	| Unary_minus expression | BracketE expression | Array_creation (_,expression) -> make_call_graph_expression false prev_identifier map expression
	| Arithmetic_operation (expression1,_,expression2) | Array_access (expression1,expression2) | Comparison (expression1,_,expression2) -> make_call_graph_expression false prev_identifier (make_call_graph_expression false prev_identifier map expression2) expression1
	| Function_call (identifier,expression_list) -> make_call_graph_expression_list prev_identifier (link terminal prev_identifier identifier map) expression_list
	| _ -> map
	
and make_call_graph_expression_list prev_identifier map expression_list =
	List.fold_left (make_call_graph_expression false prev_identifier) map expression_list;;
	
let rec make_call_graph_condition prev_identifier map condition =match condition with
	| Expr expression -> make_call_graph_expression false prev_identifier map expression
	| Negation condition | Bracket condition -> make_call_graph_condition prev_identifier map condition
	| Or (condition1,condition2) | And (condition1,condition2) -> make_call_graph_condition prev_identifier (make_call_graph_condition prev_identifier map condition2) condition1
	;;

let rec make_call_graph_instruction in_function_definition terminal prev_identifier map instr =match instr with
	| Proc_call (identifier,expression_list) -> make_call_graph_expression_list prev_identifier (link terminal prev_identifier identifier map) expression_list
	| Var_assign (_,expression) -> make_call_graph_expression (terminal && in_function_definition) prev_identifier map expression
	| Array_assign (expression1,expression2,expression3) -> make_call_graph_expression false prev_identifier (make_call_graph_expression false prev_identifier (make_call_graph_expression false prev_identifier map expression3) expression2) expression1
	| Conditional (condition,instruction1,instruction2) -> make_call_graph_condition prev_identifier (make_call_graph_instruction in_function_definition terminal prev_identifier (make_call_graph_instruction in_function_definition terminal prev_identifier map instruction2) instruction1) condition
	| Loop (condition,instruction) -> make_call_graph_condition prev_identifier (make_call_graph_instruction in_function_definition false prev_identifier map instruction) condition
	| Block instruction_list -> make_call_graph_instruction_list in_function_definition terminal prev_identifier map instruction_list
	
and make_call_graph_instruction_list in_function_definition terminal prev_identifier map instruction_list = match instruction_list with 
	| instruction::instruction2::l -> make_call_graph_instruction_list in_function_definition terminal prev_identifier (make_call_graph_instruction in_function_definition false prev_identifier map instruction) (instruction2::l)
	| instruction::[] -> make_call_graph_instruction in_function_definition terminal prev_identifier map instruction
	| [] -> map;;
	
let make_call_graph_func_def map (identifier,_,_,_,instruction_list) =
	make_call_graph_instruction_list true true identifier map instruction_list;;

let make_call_graph_proc_def map (identifier,_,_,instruction_list) =
	make_call_graph_instruction_list false true identifier map instruction_list;;

let make_call_graph_proc_or_func_def map proc_or_func_def =match proc_or_func_def with
	| Func f -> make_call_graph_func_def map f
	| Proc p -> make_call_graph_proc_def map p;;


let make_call_graph_procedure_or_function_list procedure_or_function_list =
	List.fold_left make_call_graph_proc_or_func_def IdentifierMap.empty procedure_or_function_list;;

let make_call_graph (_,procedure_or_function_list,instruction_list) = make_call_graph_instruction_list false true "program" (make_call_graph_procedure_or_function_list procedure_or_function_list) instruction_list;;
	
