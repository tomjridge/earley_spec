open Earley_spec
open Example_grammar


let len = Sys.argv.(1) |> int_of_string

(*
let main () =
  run_parser ~input:(String.make len '1')

let _ = main ()
*)

let r = 
  run_parser ~input:(String.make len '1')
  |> extract_results 
  |> results_to_string 
  |> fun rs ->
  print_endline rs;
  Tjr_file.write_string_to_file ~fn:"results.json" rs

