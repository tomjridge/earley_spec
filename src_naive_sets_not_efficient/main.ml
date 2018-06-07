open Naive_general_parser

(* various examples *)

let eps = TM 1
let x = TM 3

let parse_eps = 
  fun (s,i,j) -> if i<=j then [i] else failwith __LOC__

let parse_x = fun (s,i,j) ->
  (* this terminal parser requires to know string_t *)
  let (s:string) = string_t_to_string s in  
  if i < j && i < String.length s && String.get s i = 'x' then 
    [i+1]
  else
    []

let p_of_tm = 
  fun tm -> 
    if TM tm=eps then parse_eps
    else if TM tm=x then parse_x
    else failwith __LOC__

let i0 str = 
  let len = String.length str in
  let str : string_t = string_to_string_t str in
  { str; len }


(* E -> E E E | "x" | eps *)
module E_EEE = struct

  let e' = 2
  let e = NT e'

  let nt_items_for_nt = fun nt (s,i) ->
    let _ = assert(nt=e') in
    let as_ = [] in
    let k = i in
    [{nt;i;as_;k;bs=[e;e;e]};
     {nt;i;as_;k;bs=[x]};
     {nt;i;as_;k;bs=[eps]}]

  let grammar = {nt_items_for_nt; p_of_tm}

  let c0 str = {grammar;input=(i0 str)}

  let sym_to_string = function
    | TM 1 -> "eps"
    | TM 3 -> "x"
    | NT 2 -> "E"
    | _ -> failwith __LOC__
end

open E_EEE

let main () = 
  let len = Sys.argv.(1) |> int_of_string in
  let str = String.make len 'x' in
  Naive_general_parser.run (c0 str) e' 
  |> extract_results
(*  |> results_to_string
  |> print_endline *)
  |> List.map (itm_to_string ~sym_to_string)
  |> List.iter print_endline

;;

main ()

;;

