old:
	ocamlbuild -I ptrees -use-ocamlfind -package qcheck qctest.native
	ocamlbuild -I ptrees -use-ocamlfind -package qcheck qctest.cma

new:
	ocamlbuild -X ptrees -I ptset-master -use-ocamlfind -package qcheck qctest.native
	ocamlbuild -X ptrees -I ptset-master -use-ocamlfind -package qcheck qctest.cma

opam:
	ocamlbuild -X ptrees -use-ocamlfind -package qcheck,ptset qctest.native
	ocamlbuild -X ptrees -use-ocamlfind -package qcheck,ptset qctest.cma

clean:
	ocamlbuild -clean
