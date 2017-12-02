all: mapfold expression mapfold_tests expression_tests

mapfold: mapfold.ml
	ocamlbuild mapfold.byte

expression: expression.ml
	ocamlbuild mapfold.byte

expression_tests: expression_tests.ml
	ocamlbuild expression_tests.byte

mapfold_tests: mapfold_tests.ml
	ocamlbuild mapfold_tests.byte

clean:
	rm -rf _build *.byte