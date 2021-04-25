PKGS=opal
SRCS=graph.ml flow_network.ml ast.ml eval.ml util.ml blade.ml parser.ml main.ml

.PHONY: docs clean

main.native: $(SRCS)
	ocamlbuild -pkgs '${PKGS}' main.native

docs: $(SRCS)
	mkdir -p docs
	ocamlfind ocamlc -linkpkg -package opal $^
	ocamlfind ocamldoc -html -d docs -package opal $^

clean:
	-rm *.cmi *.cmo *.cma *.native a.out

all: main.native