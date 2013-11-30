(** Test the parser *)

open Type;;
open Parser;;

Parser.main Lexer.token (Lexing.from_channel stdin);;
