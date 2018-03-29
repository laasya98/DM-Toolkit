test:
	ocamlbuild -use-ocamlfind

compile-i:
	ocamlc -c state.mli database.mli character.mli event.mli 

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
