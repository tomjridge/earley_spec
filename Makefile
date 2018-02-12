all:
	ocamlfind ocamlopt -package tjr_lib,ppx_deriving_yojson -linkpkg -o earley_spec.native earley_spec.ml
