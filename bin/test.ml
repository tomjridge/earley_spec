open Earley_spec

(* example grammar -------------------------------------------------- *)

let _E = 99
let _1 = 1
let eps = 0

let expand_nt (i,_X) s = 
  let new_items = 
    if _X = _E then 
      [[NT _E;NT _E;NT _E];[TM _1];[TM eps]]
    else 
      failwith __LOC__
  in
  new_items 
  |> List.map (fun bs -> Raw {nt=_E;i;as_=[];k=i;bs})
  |> fun itms ->
  Tjr_list.with_each_elt
    ~step:(fun ~state itm -> add_item itm state)
    ~init_state:s
    itms

let expand_tm ~input (i,_T) s = 
  match _T with
  | _ when _T = _1 ->
    if i < String.length input && String.get input i = '1' then 
      add_item (Cut_complete (i,TM _T,i+1)) s
    else s
  | _ when _T = eps ->
    add_item (Cut_complete (i,TM _T,i)) s
  | _ -> failwith __LOC__
    
let expand ~input (i,_S) s = 
  match _S with
  | TM _T -> expand_tm ~input (i,_T) s
  | NT _X -> expand_nt (i,_X) s

let run_parser ~input = run_parser ~input ~expand:(expand ~input) ~init_nonterm:_E

;;

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

