{open Parser}
rule token = parse
	[' ' '\r' '\t' '\n'] {token lexbuf}
	| ['1'-'9']['0'-'9']*|'0' as i {INT(int_of_string(i))}
	| ":=" {ASSIGNMENT}
	| ':' {DOUBLE_POINT}
	| '(' {LEFT_BRACKET}
	| ')' {RIGHT_BRACKET}
	| '[' {LEFT_SQUARED_BRACKET}
	| ']' {RIGHT_SQUARED_BRACKET}
	| "new array of" {NEW_ARRAY_OF}
	| "not" {NOT}
	| "or" {OR}
	| "and" {AND}
	| "<>" {DIFF}
	| ">=" {SUP_EQUAL}
	| '>' {SUP}
	| "<=" {INF_EQUAL}
	| '=' {EQUAL}
	| '<' {INF}
	| '/' {DIV}
	| '*' {MULT}
	| '-' {MINUS}
	| '+' {PLUS}
	| "false" {FALSE}
	| "true" {TRUE}
	| "while" {WHILE}
	| "do" {DO}
	| "else" {ELSE}
	| "then" {THEN}
	| "if" {IF}
	| "end" {END}
	| "begin" {BEGIN}
	| "program" {PROGRAM}
	| "function" {FUNCTION}
	| "procedure" {PROCEDURE}
	| "var" {VAR}
	| ';' {SEMICOLON}
	| '.' {DOT}
	| ',' {COMMA}
	| "array of" {ARRAY_OF}
	| "boolean" {BOOLEAN}
	| "integer" {INTEGER}
	| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as i {IDENTIFIER(i)}
	| '{' [^'{' '}']+ '}' {token lexbuf}
	| eof {EOF}
