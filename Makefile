test:
	ocamlbuild -use-ocamlfind

compile-i:
	ocamlc -c database.mli item.mli character.mli event.mli state.mli

compile:
	ocamlbuild -use-ocamlfind event.cmo state.cmo

check:
	bash checkenv.sh

zip:
	zip dm-toolkitsrc.zip *.ml*

zipcheck:
	bash checkzip.sh

clean:
	ocamlbuild -clean
	rm -f dm-toolkitsrc.zip
