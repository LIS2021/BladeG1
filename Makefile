PKGS=opal

.PHONY: docs clean

main.native: ast.ml util.ml blade.ml parser.ml graph.ml flow_network.ml main.ml
	ocamlbuild -pkgs '${PKGS}' main.native

docs: graph.ml flow_network.ml ast.ml parser.ml blade.ml util.ml main.ml
	mkdir -p docs
	ocamlfind ocamlc -linkpkg -package opal $^
	ocamlfind ocamldoc -html -d docs -package opal $^

clean:
	-rm *.cmi *.cmo *.cma *.native a.out

all: main.native