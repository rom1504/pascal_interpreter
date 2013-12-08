(** Convert program to a string which contain a program *)

open Type;;
open Parser;;
open String_of_pascal;;

String_of_pascal.pretty_print (Parser.main Lexer.token (Lexing.from_channel stdin));;
print_newline();;