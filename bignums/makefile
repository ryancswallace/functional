all: ps3 ps3_tests

ps3: ps3.ml
	ocamlbuild ps3.byte

ps3_tests: ps3_tests.ml
	ocamlbuild ps3_tests.byte

clean:
	rm -rf _build *.byte