PKGS=opal
SRCS=graph.ml flow_network.ml ast.ml def_use_generator.ml eval.ml util.ml blade.ml parser.ml do_blade.ml

.PHONY: docs clean all

%.native: $(SRCS)
	ocamlbuild -pkgs '${PKGS}' -tag 'debug' $@

docs: $(SRCS)
	mkdir -p docs
	ocamlfind ocamlc -linkpkg -package opal $^
	ocamlfind ocamldoc -html -d docs -package opal $^
	make clean

clean:
	-rm *.cmi *.cmo *.cma *.native a.out

all: do_blade.native