PKGS=opal

ast.native: ast.ml graph.ml flow_network.ml
	ocamlbuild -pkgs '${PKGS}' ast.native