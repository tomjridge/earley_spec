open Earley_spec
open Example_grammar


let len = Sys.argv.(1) |> int_of_string

(*
let main () =
  run_parser ~input:(String.make len '1')

let _ = main ()
*)

let r = 
  let s = run_parser ~input:(String.make len '1') in
  let rs = extract_results s in
  Printf.printf "Finished (count: %d)\n" (List.length rs)

