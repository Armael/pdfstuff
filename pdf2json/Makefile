all: pdf2json

pdf2json: pdf2json.ml
	ocamlbuild -use-ocamlfind pdf2json.native
	cp -H $@.native $@
	rm $@.native
