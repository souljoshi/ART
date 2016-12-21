# Make sure ocamlbuild can find opam-managed packages: first run
#
# eval `opam config env`

# Easiest way to build: using ocamlbuild, which in turn uses ocamlfind
		
.PHONY : art.native

art.native:
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis -cflags -w,+a-4 \
		art.native

# More detailed: build using ocamlc/ocamlopt + ocamlfind to locate LLVM

OBJS = ast.cmx codegen.cmx parser.cmx scanner.cmx semant.cmx art.cmx

art : $(OBJS)
	ocamlfind ocamlopt -linkpkg -package llvm -package llvm.analysis $(OBJS) -o art

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

%.cmx : %.ml
	ocamlfind ocamlopt -c -package llvm $<

### Generated by "ocamldep *.ml *.mli" after building scanner.ml and parser.ml
art.cmo : semant.cmo scanner.cmo parser.cmi codegen.cmo ast.cmo
art.cmx : semant.cmx scanner.cmx parser.cmx codegen.cmx ast.cmx
ast.cmo :
ast.cmx :
codegen.cmo : ast.cmo
codegen.cmx : ast.cmx
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmx parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx
semant.cmo : ast.cmo
semant.cmx : ast.cmx
parser.cmi : ast.cmo


# "make clean" removes all generated files
.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log *.diff art scanner.ml parser.ml parser.mli
	rm -rf *.cmx *.cmi *.cmo *.cmx *.o

.PHONY: clean-tests

clean-tests:
	rm -rf results
	rm -f *.diff *.ll *.out *.err

# Building the tarball

TESTS = *

FAILS = *

DEMOS = *

TESTFILES = $(TESTS:%=test-%.out) $(TESTS:%=test-%.art)\
	    $(FAILS:%=fail-%.art) $(FAILS:%=fail-%.err)

DEMOFILES = $(DEMOS)

TARFILES = ast.ml codegen.ml compile Makefile art.ml parser.mly README.md \
	scanner.mll semant.ml testall.sh $(TESTFILES:%=tests/%) $(DEMOFILES:%=demos/%)

ART.tar.gz : $(TARFILES)
	cd .. && tar czf ART/ART.tar.gz \
		$(TARFILES:%=ART/%) 
