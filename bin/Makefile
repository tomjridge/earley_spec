t_no:=test_no_postprocess

all:
	ocamlfind ocamlopt -package tjr_lib,ppx_deriving_yojson,earley_spec -linkpkg -o test.native test.ml
	ocamlfind ocamlopt -package tjr_lib,ppx_deriving_yojson,earley_spec -linkpkg -o $(t_no).native $(t_no).ml

p:=./$(t_no).native
timings:
	time $(p) 100
	time $(p) 200
	time $(p) 300
	time $(p) 400
