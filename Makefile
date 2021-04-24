PKGS=opal

.PHONY: docs clean

ast.native: ast.ml graph.ml flow_network.ml
	ocamlbuild -pkgs '${PKGS}' ast.native

docs: graph.ml flow_network.ml ast.ml
	mkdir -p docs
	ocamlfind ocamlc -linkpkg -package opal $^
	ocamlfind ocamldoc -html -d docs -package opal $^

clean:
	-rm *.cmi *.cmo *.cma *.native a.out

all: ast.native