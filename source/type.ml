(** Types to define an abstract syntax tree *)

type constant = Int of int | Bool of bool

type identifier = string

type op = Plus | Minus | Mult | Div 

type comp = Inf | Inf_equal | Sup | Sup_equal | Equal | Diff 

type pascal_type = Integer | Boolean | Array of pascal_type

type expression =
	Const of constant
	| Var of identifier
	| Unary_minus of expression
	| BracketE of expression
	| Arithmetic_operation of expression*op*expression
	| Comparison of expression*comp*expression
	| Function_call of identifier*(expression list)
	| Array_access of expression*expression
	| Array_creation of pascal_type*expression

type condition =
	Expr of expression
	| Negation of condition
	| Or of condition*condition
	| And of condition*condition
	| Bracket of condition

	
type environment = ((identifier list)*pascal_type) list

type instruction =
	Proc_call of identifier*(expression list)
	| Var_assign of identifier*expression
	| Array_assign of expression*expression*expression
	| Conditional of condition*instruction*instruction
	| Loop of condition*instruction
	| Block of instruction list
	
type procedure = identifier*environment*environment*(instruction list)

type pascal_function = identifier*environment*pascal_type*environment*(instruction list)

type procedure_or_function =
	Proc of procedure
	| Func of pascal_function

type program = environment*(procedure_or_function list)*(instruction list)
