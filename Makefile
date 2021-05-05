PKGS=opal llvm llvm.bitwriter
SRCS=graph.ml flow_network.ml ast.ml def_use_generator.ml \
	vm_types.ml eval.ml util.ml blade.ml parser.ml do_blade.ml \
	run_interp.ml run_vm.ml gen_llvm.ml
TEST_SOURCES = test/test_fetch.ml test/test_exec.ml test/test_retire.ml
TEST_RESULT = test/test_fetch.native test/test_exec.native test/test_retire.native
TESTS = test test2 test3
LINK=llvm-link
LLC=llc
AS=llvm-as
CLANG=clang

.PHONY: docs clean all

all: do_blade.native run_interp.native run_vm.native gen_llvm.native battery_test

%.native: $(SRCS) $(TEST_SOURCES)
	ocamlbuild -pkgs '${PKGS}' -tag 'debug' $@

battery_test: $(TEST_RESULT)

rt-support.bc: rt-support.c
	$(CLANG) -c $? -emit-llvm -o $@

test%.bc: test%.txt gen_llvm.native
	./gen_llvm.native -o $@ < $^

linked_test%.bc: test%.bc rt-support.bc
	$(LINK) $? -o $@

test%.o: linked_test%.bc
	$(LLC) -filetype=obj $? -o $@

test%: test%.o
	$(CLANG) $? -o $@

docs: $(SRCS)
	mkdir -p docs
	ocamlfind ocamlc -linkpkg -package opal $^
	ocamlfind ocamldoc -html -d docs -package opal $^
	make clean

clean:
	-rm *.cmi *.cmo *.cma *.native a.out *.bc *.o *.ll
	rm -r _build 