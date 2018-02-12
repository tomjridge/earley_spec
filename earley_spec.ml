open Tjr_set
open Tjr_map

type nonterm
type terminal
type term = terminal

type symbol = NT of nonterm | TM of terminal

type raw_item = { nt:nonterm; i:int; as_:symbol list; k:int; bs:symbol list }

type item = 
  | Cut_complete of int * symbol * int
  | Cut_blocked of raw_item (* bs <> [] *)
  | Expand of int * symbol
  | Raw of raw_item

type item_set

type raw_item_set

(* complete and blocked maps are from k,S *)
type 'a map_int_symbol

type int_set

type state = {
  todo_done: item_set;
  todo: item list;
  blocked: raw_item_set map_int_symbol;
  complete: int_set map_int_symbol  (* map k,S -> j set *)
}
  

let step 
    ~item_set_ops 
    ~raw_set_ops 
    ~raw_set_to_list 
    ~complete_ops
    ~blocked_ops 
    ~int_set_ops
    ~int_set_to_list
    ~add_item 
    ~cut 
    ~expand 
    (s:state) 
  = 
  match s.todo with
  | [] -> None
  | x::xs -> 
    let s = {s with todo=xs } in
    match x with
    | Cut_complete (k,_S,j) -> (
        (* cut complete item kSj with items blocked on k,S *)
        blocked_ops.map_find (k,_S) s.blocked |> function
        | None -> Some s
        | Some bs -> 
          (* process each of these blocked items against the complete item *)
          bs 
          |> raw_set_to_list (* FIXME possible inefficiency *)
          |> Tjr_list.with_each_elt
            ~init_state:s
            ~step:(fun ~state blocked_item ->
                let new_item = cut blocked_item j in
                add_item new_item s)
          |> fun x -> Some x)
    | Cut_blocked blocked_item -> (
        let (k,_S) = (blocked_item.k, List.hd blocked_item.bs) in
        complete_ops.map_find (k,_S) s.complete |> fun js ->
        js 
        |> int_set_to_list
        |> Tjr_list.with_each_elt
          ~init_state:s
          ~step:(fun ~state j ->
              let new_item = cut blocked_item j in
              add_item new_item s)
        |> fun s -> Some s)
    | Expand (i,_S) -> (expand (i,_S) s)  
    | Raw itm ->
      match itm.bs with
      | [] -> 
        Some (add_item (Cut_complete (itm.i,NT itm.nt,itm.k)) s)
      | _S::bs -> 
        s |> add_item (Expand (itm.k,_S)) |> add_item (Cut_blocked itm) 
        |> fun s -> Some s


