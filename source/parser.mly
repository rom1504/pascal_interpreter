%{open Type;;%}
%token TRUE FALSE
%token <string> IDENTIFIER
%token <int> INT
%token LEFT_BRACKET RIGHT_BRACKET
%token LEFT_SQUARED_BRACKET RIGHT_SQUARED_BRACKET
%token COMMA DOUBLE_POINT DOT
%token ASSIGNMENT
%token NEW_ARRAY_OF
%token NOT OR AND
%token DIFF SUP_EQUAL SUP INF_EQUAL EQUAL INF
%token DIV MULT MINUS PLUS
%token FALSE TRUE
%token WHILE ELSE THEN IF END DO
%token BEGIN PROGRAM FUNCTION PROCEDURE
%token VAR
%token SEMICOLON
%token ARRAY_OF
%token BOOLEAN INTEGER
%token EOF

%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS

%left OR
%left AND
%nonassoc PBRACKET

%left SEMICOLON
%nonassoc USEMICOLON



%start main
%type <Type.program> main
%%

main:program EOF {$1};

program:
	PROGRAM
	optional_variable_definition
	procedure_or_function_definition_list
	BEGIN
	instruction_list
	END DOT
	{($2,$3,$5)};
	
optional_variable_definition:
	variable_definition {$1}
	| {[]};
	
instruction:
	IDENTIFIER LEFT_BRACKET expression_list RIGHT_BRACKET {Proc_call($1,$3)}
	| IDENTIFIER ASSIGNMENT expression {Var_assign($1,$3)}
	| expression LEFT_SQUARED_BRACKET expression RIGHT_SQUARED_BRACKET ASSIGNMENT expression {Array_assign($1,$3,$6)}
	| IF condition THEN instruction ELSE instruction {Conditional($2,$4,$6)}
	| WHILE condition DO instruction {Loop($2,$4)}
	| BEGIN instruction_list END {Block $2};

expression_list:
	expression COMMA expression_list {$1::$3}
	|expression {$1::[]}
	|{[]};
	
condition:
	expression {Expr $1}
	| NOT condition {Negation $2}
	| condition OR condition {Or($1,$3)}
	| condition AND condition {And($1,$3)}
	| LEFT_BRACKET condition RIGHT_BRACKET %prec PBRACKET {Bracket $2};
	
expression:
	constant {Const $1}
	| IDENTIFIER {Var $1}
	| LEFT_BRACKET expression RIGHT_BRACKET {BracketE $2}
	| MINUS expression %prec UMINUS {Unary_minus $2}
	| expression op expression {Arithmetic_operation($1,$2,$3)}
	| expression comp expression {Comparison($1,$2,$3)}
	| IDENTIFIER LEFT_BRACKET expression_list RIGHT_BRACKET {Function_call($1,$3)}
	| expression LEFT_SQUARED_BRACKET expression RIGHT_SQUARED_BRACKET {Array_access($1,$3)}
	| NEW_ARRAY_OF pascal_type LEFT_SQUARED_BRACKET expression RIGHT_SQUARED_BRACKET  {Array_creation($2,$4)};

constant:
	INT {Int $1}
	| TRUE {Bool true}
	| FALSE {Bool false};
	
op:
	PLUS {Plus}
	| MINUS {Minus}
	| MULT {Mult}
	| DIV {Div};
	
comp:
	INF {Inf}
	| INF_EQUAL {Inf_equal}
	| SUP {Sup}
	| SUP_EQUAL {Sup_equal}
	| EQUAL {Equal}
	| DIFF {Diff};
	 
	
function_definition:
	FUNCTION IDENTIFIER LEFT_BRACKET environment RIGHT_BRACKET DOUBLE_POINT pascal_type SEMICOLON
	optional_variable_definition
	BEGIN
	instruction_list
	END SEMICOLON
	{($2,$4,$7,$9,$11)};
	
procedure_definition:
	PROCEDURE IDENTIFIER LEFT_BRACKET  environment RIGHT_BRACKET SEMICOLON
	optional_variable_definition
	BEGIN
	instruction_list
	END SEMICOLON
	{($2,$4,$7,$9)};
	
procedure_or_function_definition:
	function_definition {Func $1}
	| procedure_definition {Proc $1};

instruction_list:
	instruction SEMICOLON instruction_list {$1::$3}
	| instruction {$1::[]}
	| {[]};	

procedure_or_function_definition_list:
	procedure_or_function_definition procedure_or_function_definition_list {$1::$2}
	| procedure_or_function_definition {$1::[]}
	| {[]};		
	
variable_definition:VAR non_empty_environment_semicolon {$2};

non_empty_environment_semicolon : sub_environment_semicolon non_empty_environment_semicolon {$1::$2}
	| sub_environment_semicolon {$1::[]};

sub_environment_semicolon : sub_environment SEMICOLON {$1};

environment:
	sub_environment SEMICOLON environment {$1::$3}
	| sub_environment {$1::[]}
	| {[]};
	
sub_environment:
	identifier_list DOUBLE_POINT pascal_type {($1,$3)};
	
pascal_type:
	INTEGER {Integer}
	| BOOLEAN {Boolean}
	| ARRAY_OF pascal_type {Array $2};

identifier_list:
	IDENTIFIER COMMA identifier_list {$1::$3}
	| IDENTIFIER {$1::[]};
