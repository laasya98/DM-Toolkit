test:
	ocamlbuild -use-ocamlfind 

compile:
	ocamlbuild -use-ocamlfind *.cmo

check:
	bash checkenv.sh

zip:
	zip dm-toolkitsrc.zip *.ml*

zipcheck:
	bash checkzip.sh

clean:
	ocamlbuild -clean
	rm -f dm-toolkitsrc.zip
