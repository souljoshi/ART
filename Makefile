# basic makefile

OBJS =  parser.cmo scanner.cmo ast.cmo semant.cmo codegen.cmo art.cmo

art: scanner parser ast semant codegen
	ocamlc -c parser.mli  # compile parser types
	ocamlc -c scanner.ml  # compile the scanner
	ocamlc -c parser.ml   # compile the parser
	ocamlfind ocamlc -package llvm -c art.ml       # compile top level
	ocamlfind ocamlc -linkpkg -package llvm -package llvm.analysis -o art $(OBJS)

scanner: scanner.mll
	ocamllex scanner.mll  # create scanner.ml

parser: parser.mly
	ocamlyacc parser.mly  # create parser.ml and parser.mli

ast: ast.ml
	ocamlc -c ast.ml     # compile AST types

semant: semant.ml
	ocamlfind ocamlc -package llvm -c semant.ml    # compile semant

codegen: codegen.ml
	ocamlfind ocamlc -package llvm -c codegen.ml   # compile codegen

.PHONY: clean

clean:
	rm -f *.cmo *.cmi scanner.ml parser.ml parser.mli parser.output art
