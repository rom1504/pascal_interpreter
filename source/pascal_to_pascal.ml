(** Permet de convertir un program en une string contenant un programme *)

open Type;;
open Parser;;

let pretty_print a=print_string "";;

pretty_print (Parser.main Lexer.token (Lexing.from_channel stdin));;
print_newline();;