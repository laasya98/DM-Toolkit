test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

compile-i:
	ocamlc -c global.mli command.mli character.mli event.mli database.mli state.mli

compile:
	ocamlbuild -use-ocamlfind event.cmo state.cmo character.cmo global.cmo

play:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte

check:
	bash checkenv.sh

zip:
	zip dm-toolkitsrc.zip *.ml* data/*

zipcheck:
	bash checkzip.sh

clean:
	ocamlbuild -clean
	rm -rf save
	rm -f dm-toolkitsrc.zip
