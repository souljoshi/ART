# basic makefile

art: scanner parser ast
	ocamlc -c parser.mli  # compile parser types
	ocamlc -c scanner.ml  # compile the scanner
	ocamlc -c parser.ml   # compile the parser
	ocamlc -c art.ml      # compile top level
	ocamlc -o art parser.cmo scanner.cmo ast.cmo art.cmo

scanner: scanner.mll
	ocamllex scanner.mll  # create scanner.ml

parser: parser.mly
	ocamlyacc parser.mly  # create parser.ml and parser.mli

ast: ast.ml
	ocamlc -c ast.ml     # compile AST types

.PHONY: clean

clean:
	rm -f *.cmo *.cmi scanner.ml parser.ml parser.mli parser.output art
