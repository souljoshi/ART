# basic makefile

art:
	ocammlex scanner.mll  # create scanner.ml
	ocamlyacc parser.mly  # create parser.ml and parser.mli
	ocamlc -c ast.mli     # compile AST types
	ocamlc -c parser.mli  # compile parser types
	ocamlc -c scanner.ml  # compile the scanner
	ocamlc -c parser.ml   # compile the parser
	ocamlc -c art.ml      # compile top level
	ocamlc -o art parser.cmo scanner.cmo calc.cmo

.PHONY: clean

clean:
	rm -f *.cmo *.cmi scanner.ml parser.ml parser.mli art
