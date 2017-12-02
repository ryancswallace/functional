all: miniml expr evaluation test

miniml: miniml.ml
	ocamlbuild miniml.byte -lib str

expr: expr.ml
	ocamlbuild expr.byte -lib str

evaluation: evaluation.ml
	ocamlbuild evaluation.byte -lib str

test: test.ml
	ocamlbuild test.byte -lib str -lib unix

clean:
	rm -rf _-build *.byte