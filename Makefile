all: bin/test_parse bin/pascal_to_pascal bin/print_call_graph

bin/print_call_graph:temp/lexer.cmo temp/parser.cmo temp/print_call_graph.cmo temp/type.cmo temp/make_call_graph.cmo Makefile
	ocamlc -I temp -o bin/print_call_graph temp/type.cmo temp/make_call_graph.cmo temp/lexer.cmo temp/parser.cmo temp/print_call_graph.cmo
	
temp/print_call_graph.cmo:source/print_call_graph.ml temp/parser.cmi temp/lexer.cmi temp/type.cmi temp/make_call_graph.cmi Makefile
	ocamlc -I temp -o temp/print_call_graph.cmo -c source/print_call_graph.ml
	
temp/make_call_graph.cmo temp/make_call_graph.cmi:source/make_call_graph.ml temp/type.cmi Makefile
	ocamlc -I temp -o temp/make_call_graph.cmo -c source/make_call_graph.ml

bin/test_parse:temp/lexer.cmo temp/parser.cmo temp/test_parse.cmo temp/type.cmo Makefile
	ocamlc -I temp -o bin/test_parse temp/type.cmo temp/lexer.cmo temp/parser.cmo temp/test_parse.cmo
	
temp/test_parse.cmo:source/test_parse.ml temp/parser.cmi temp/lexer.cmi temp/type.cmi Makefile
	ocamlc -I temp -o temp/test_parse.cmo -c source/test_parse.ml

bin/pascal_to_pascal:temp/lexer.cmo temp/parser.cmo temp/pascal_to_pascal.cmo temp/type.cmo Makefile
	ocamlc -I temp -o bin/pascal_to_pascal temp/type.cmo temp/lexer.cmo temp/parser.cmo temp/pascal_to_pascal.cmo
	
temp/pascal_to_pascal.cmo:source/pascal_to_pascal.ml temp/parser.cmi temp/lexer.cmi temp/type.cmi Makefile
	ocamlc -I temp -o temp/pascal_to_pascal.cmo -c source/pascal_to_pascal.ml

temp/parser.cmo:temp/parser.ml temp/parser.cmi temp/type.cmi Makefile
	ocamlc -I temp -o temp/parser.cmo -c temp/parser.ml
	
temp/lexer.cmo temp/lexer.cmi:temp/lexer.ml temp/parser.cmi temp/type.cmi  Makefile
	ocamlc -I temp -o temp/lexer.cmo -c temp/lexer.ml
	
temp/type.cmo temp/type.cmi:source/type.ml Makefile
	ocamlc -I temp -o temp/type.cmo -c source/type.ml
	
temp/parser.cmi:temp/parser.mli temp/type.cmi Makefile
	ocamlc  -I temp -o temp/parser.cmi -c temp/parser.mli

temp/lexer.ml:source/lexer.mll Makefile
	ocamllex -o temp/lexer.ml source/lexer.mll
	
temp/parser.ml temp/parser.mli:source/parser.mly temp/type.cmi Makefile
	ocamlyacc -b temp/parser source/parser.mly
	
clean:
	rm -f temp/*
	rm -f bin/*
	rm -f htmldoc/*
	
htmldoc:source/make_call_graph.ml  source/pascal_to_pascal.ml  source/print_call_graph.ml  source/test_parse.ml  source/type.ml temp/lexer.cmi  temp/make_call_graph.cmi  temp/parser.cmi  temp/pascal_to_pascal.cmi  temp/print_call_graph.cmi  temp/test_parse.cmi  temp/type.cmi Makefile
	mkdir -p htmldoc
	ocamldoc -charset utf8 -html -d htmldoc -I temp source/make_call_graph.ml  source/pascal_to_pascal.ml  source/print_call_graph.ml  source/test_parse.ml  source/type.ml
	
pascal_to_pascal_example:bin/pascal_to_pascal Makefile
	@echo "Premier exemple, trivial.p original :"
	@cat exemple/trivial.p
	@echo ""
	@echo "Premier exemple, trivial.p généré :"
	@cat exemple/trivial.p | bin/pascal_to_pascal
	@echo ""
	
parse_test:temp/parse_error_test_done
	

pascal_to_pascal_test:temp/pascal_to_pascal_test_done
	

print_call_graph_test:temp/print_call_graph_test_done
	
	
temp/parse_error_test_done:bin/test_parse test/parse_error_test.sh Makefile
	bash test/parse_error_test.sh
	touch temp/parse_error_test_done
	
temp/pascal_to_pascal_test_done:bin/pascal_to_pascal test/pascal_to_pascal_test.sh Makefile
	bash test/pascal_to_pascal_test.sh
	touch temp/pascal_to_pascal_test_done
	
temp/print_call_graph_test_done:bin/print_call_graph test/print_call_graph_test.sh Makefile
	bash test/print_call_graph_test.sh
	touch temp/print_call_graph_test_done

test:parse_test pascal_to_pascal_test print_call_graph_test
	