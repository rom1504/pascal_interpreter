all: bin/pascal_to_pascal

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
	
htmldoc:source/type.ml source/pascal_to_pascal.ml temp/type.cmi temp/parser.cmi temp/infer_type.cmi temp/lexer.cmi Makefile
	ocamldoc -charset utf8 -html -d htmldoc -I temp source/type.ml source/pascal_to_pascal.ml
	
test_pascal_to_pascal:bin/pascal_to_pascal Makefile
	@echo "Premier exemple, trivial.p :"
	@cat exemple/trivial.p | bin/pascal_to_pascal
	@echo ""
