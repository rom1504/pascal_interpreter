CC=ocamlopt
OBJ=cmx

all: bin/test_parse bin/pascal_to_pascal bin/print_call_graph bin/pascal_interpreter bin/pascal_to_c

bin/pascal_interpreter:temp/lexer.$(OBJ) temp/parser.$(OBJ) temp/pascal_interpreter.$(OBJ) temp/type.$(OBJ) temp/string_of_pascal.$(OBJ) Makefile bin temp
	$(CC) -I temp -o bin/pascal_interpreter temp/type.$(OBJ) temp/lexer.$(OBJ) temp/parser.$(OBJ) temp/string_of_pascal.$(OBJ) temp/pascal_interpreter.$(OBJ)
	
temp/pascal_interpreter.$(OBJ):source/pascal_interpreter.ml temp/parser.cmi temp/lexer.cmi temp/type.cmi temp/string_of_pascal.cmi Makefile bin temp
	$(CC) -I temp -o temp/pascal_interpreter.$(OBJ) -c source/pascal_interpreter.ml

bin/print_call_graph:temp/lexer.$(OBJ) temp/parser.$(OBJ) temp/print_call_graph.$(OBJ) temp/type.$(OBJ) temp/make_call_graph.$(OBJ) Makefile bin temp
	$(CC) -I temp -o bin/print_call_graph temp/type.$(OBJ) temp/make_call_graph.$(OBJ) temp/lexer.$(OBJ) temp/parser.$(OBJ) temp/print_call_graph.$(OBJ)
	
temp/print_call_graph.$(OBJ):source/print_call_graph.ml temp/parser.cmi temp/lexer.cmi temp/type.cmi temp/make_call_graph.cmi Makefile bin temp
	$(CC) -I temp -o temp/print_call_graph.$(OBJ) -c source/print_call_graph.ml
	
temp/make_call_graph.$(OBJ) temp/make_call_graph.cmi:source/make_call_graph.ml temp/type.cmi Makefile bin temp
	$(CC) -I temp -o temp/make_call_graph.$(OBJ) -c source/make_call_graph.ml

bin/test_parse:temp/lexer.$(OBJ) temp/parser.$(OBJ) temp/test_parse.$(OBJ) temp/type.$(OBJ) Makefile bin temp
	$(CC) -I temp -o bin/test_parse temp/type.$(OBJ) temp/lexer.$(OBJ) temp/parser.$(OBJ) temp/test_parse.$(OBJ)
	
temp/test_parse.$(OBJ):source/test_parse.ml temp/parser.cmi temp/lexer.cmi temp/type.cmi Makefile bin temp
	$(CC) -I temp -o temp/test_parse.$(OBJ) -c source/test_parse.ml

bin/pascal_to_c:temp/lexer.$(OBJ) temp/parser.$(OBJ) temp/pascal_to_c.$(OBJ) temp/type.$(OBJ) Makefile bin temp
	$(CC) -I temp -o bin/pascal_to_c temp/type.$(OBJ) temp/lexer.$(OBJ) temp/parser.$(OBJ) temp/pascal_to_c.$(OBJ)
	
temp/pascal_to_c.$(OBJ):source/pascal_to_c.ml temp/parser.cmi temp/lexer.cmi temp/type.cmi Makefile bin temp
	$(CC) -I temp -o temp/pascal_to_c.$(OBJ) -c source/pascal_to_c.ml
	
bin/pascal_to_pascal:temp/lexer.$(OBJ) temp/parser.$(OBJ) temp/pascal_to_pascal.$(OBJ) temp/string_of_pascal.$(OBJ) temp/type.$(OBJ) Makefile bin temp
	$(CC) -I temp -o bin/pascal_to_pascal temp/type.$(OBJ) temp/lexer.$(OBJ) temp/parser.$(OBJ) temp/string_of_pascal.$(OBJ) temp/pascal_to_pascal.$(OBJ)
	
temp/pascal_to_pascal.$(OBJ):source/pascal_to_pascal.ml temp/parser.cmi temp/lexer.cmi temp/string_of_pascal.cmi temp/type.cmi Makefile bin temp
	$(CC) -I temp -o temp/pascal_to_pascal.$(OBJ) -c source/pascal_to_pascal.ml

temp/string_of_pascal.$(OBJ) temp/string_of_pascal.cmi:source/string_of_pascal.ml temp/type.cmi  Makefile bin temp
	$(CC) -I temp -o temp/string_of_pascal.$(OBJ) -c source/string_of_pascal.ml
	
temp/parser.$(OBJ):temp/parser.ml temp/parser.cmi temp/type.cmi Makefile bin temp
	$(CC) -I temp -o temp/parser.$(OBJ) -c temp/parser.ml
	
temp/lexer.$(OBJ) temp/lexer.cmi:temp/lexer.ml temp/parser.cmi temp/type.cmi  Makefile bin temp
	$(CC) -I temp -o temp/lexer.$(OBJ) -c temp/lexer.ml
	
temp/type.$(OBJ) temp/type.cmi:source/type.ml Makefile bin temp
	$(CC) -I temp -o temp/type.$(OBJ) -c source/type.ml
	
temp/parser.cmi:temp/parser.mli temp/type.cmi Makefile bin temp
	$(CC)  -I temp -o temp/parser.cmi -c temp/parser.mli

temp/lexer.ml:source/lexer.mll Makefile bin temp
	ocamllex -o temp/lexer.ml source/lexer.mll
	
temp/parser.ml temp/parser.mli:source/parser.mly temp/type.cmi Makefile bin temp
	ocamlyacc -b temp/parser source/parser.mly
	
clean:
	rm -rf temp
	rm -rf bin
	rm -rf htmldoc
	
htmldoc:source/pascal_interpreter.ml source/make_call_graph.ml  source/pascal_to_pascal.ml  source/print_call_graph.ml  source/test_parse.ml  source/type.ml temp/pascal_interpreter.cmi temp/lexer.cmi  temp/make_call_graph.cmi  temp/parser.cmi  temp/pascal_to_pascal.cmi  temp/print_call_graph.cmi  temp/test_parse.cmi  temp/type.cmi Makefile
	mkdir -p htmldoc
	ocamldoc -charset utf8 -html -d htmldoc -I temp source/pascal_interpreter.ml source/make_call_graph.ml  source/pascal_to_pascal.ml  source/print_call_graph.ml  source/test_parse.ml  source/type.ml
	
pascal_to_pascal_example:bin/pascal_to_pascal Makefile bin temp
	@echo "Premier exemple, trivial.p original :"
	@cat exemple/trivial.p
	@echo ""
	@echo "Premier exemple, trivial.p généré :"
	@cat exemple/trivial.p | bin/pascal_to_pascal
	@echo ""
	
parse_test:temp/parse_error_test_done
	

pascal_to_pascal_test:temp/pascal_to_pascal_test_done
	

print_call_graph_test:temp/print_call_graph_test_done


pascal_interpreter_all_test:temp/pascal_interpreter_all_test_done


pascal_to_c_all_test:temp/pascal_to_c_all_test_done

bin:
	mkdir bin
	
temp:
	mkdir temp
	
	
temp/parse_error_test_done:bin/test_parse test/parse_error_test.sh exemple/*.p Makefile bin temp
	bash test/parse_error_test.sh
	touch temp/parse_error_test_done
	
temp/pascal_to_pascal_test_done:bin/pascal_to_pascal test/pascal_to_pascal_test.sh exemple/*.p Makefile bin temp
	bash test/pascal_to_pascal_test.sh
	touch temp/pascal_to_pascal_test_done
	
temp/print_call_graph_test_done:bin/print_call_graph test/print_call_graph_test.sh exemple/*.p  resultats_attendus/*.dot Makefile bin temp
	bash test/print_call_graph_test.sh
	touch temp/print_call_graph_test_done
	
temp/pascal_interpreter_all_test_done:bin/pascal_interpreter test/pascal_interpreter_test.sh test/pascal_interpreter_all_test.sh exemple/*.p unit_test/*.test Makefile bin temp
	bash test/pascal_interpreter_all_test.sh
	touch temp/pascal_interpreter_all_test_done
	
temp/pascal_to_c_all_test_done:bin/pascal_to_c test/pascal_to_c_test.sh test/pascal_to_c_all_test.sh exemple/*.p unit_test/*.test Makefile bin temp
	bash test/pascal_to_c_all_test.sh
	touch temp/pascal_to_c_all_test_done

test:parse_test pascal_to_pascal_test print_call_graph_test pascal_interpreter_all_test pascal_to_c_all_test
	