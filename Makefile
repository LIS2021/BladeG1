PKGS=opal llvm
SRCS=graph.ml flow_network.ml ast.ml def_use_generator.ml \
	vm_types.ml eval.ml util.ml blade.ml parser.ml do_blade.ml \
	run_interp.ml run_vm.ml gen_llvm.ml
TEST_SOURCES = test/test_fetch.ml test/test_exec.ml test/test_retire.ml
TEST_RESULT = test/test_fetch.native test/test_exec.native test/test_retire.native

.PHONY: docs clean all

%.native: $(SRCS) $(TEST_SOURCES)
	ocamlbuild -pkgs '${PKGS}' -tag 'debug' $@

test: $(TEST_RESULT)

docs: $(SRCS)
	mkdir -p docs
	ocamlfind ocamlc -linkpkg -package opal $^
	ocamlfind ocamldoc -html -d docs -package opal $^
	make clean

clean:
	-rm *.cmi *.cmo *.cma *.native a.out
	rm -r _build

all: do_blade.native run_interp.native run_vm.native test