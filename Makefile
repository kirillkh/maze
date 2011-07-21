maze:: build.ml main.ml
		ocamlfind ocamlc -package camlimages -linkpkg -o maze build.ml main.ml

clean:
		rm *.cmi *.cmo maze
