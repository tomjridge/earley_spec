#require "ppx_deriving_yojson";;
#require "tjr_lib";;

open Tjr_set
open Tjr_map


type nonterm = int  [@@deriving yojson]
type terminal = int  [@@deriving yojson]

type symbol = NT of nonterm | TM of terminal  [@@deriving yojson]

type raw_item = 
  { nt:nonterm; i:int; as_:symbol list; k:int; bs:symbol list }
[@@deriving yojson]

type item = 
  | Cut_complete of int * symbol * int
  | Cut_blocked of raw_item (* bs <> [] *)
  | Expand of int * symbol
  | Raw of raw_item
[@@deriving yojson]

type results = item list  [@@deriving yojson]

(* complete and blocked maps are from k,S *)
type 'a map_int_symbol

type ('item_set,'raw_item_set,'int_set,'blocked,'complete) state = {
  todo_done: 'item_set;
  todo: item list;
  blocked: 'blocked;  (* map k,S -> raw_item set *)
  complete: 'complete; (* map k,S -> j set *)
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
    s 
  = 
  match s.todo with
  | [] -> None
  | x::xs -> 
    let s = {s with todo=xs } in
    match x with
    | Cut_complete (k,_S,j) -> (
        (* add the item to the complete map *)
        let key = (k,_S) in
        let s = 
          let js =
            complete_ops.map_find key s.complete |> function
            | None -> int_set_ops.empty ()
            | Some x -> x
          in
          js |> int_set_ops.add j |> fun js ->
          complete_ops.map_add key js s.complete |> fun complete -> 
          {s with complete } 
        in
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
                let new_item = Raw(cut blocked_item j) in
                add_item new_item s)
          |> fun x -> Some x)
    | Cut_blocked blocked_item -> (
        let (k,_S) = (blocked_item.k, List.hd blocked_item.bs) in
        (* add the item to the blocked map *)
        let key = (k,_S) in
        let s = 
          let itms =
            blocked_ops.map_find key s.blocked |> function
            | None -> raw_set_ops.empty ()
            | Some x -> x
          in
          raw_set_ops.add blocked_item itms |> fun itms ->
          blocked_ops.map_add key itms s.blocked |> fun blocked ->
          {s with blocked } 
        in
        complete_ops.map_find (k,_S) s.complete |> function
        | None -> Some s
        | Some js -> 
          js
          |> int_set_to_list
          |> Tjr_list.with_each_elt
            ~init_state:s
            ~step:(fun ~state j ->
                let new_item = Raw(cut blocked_item j) in
                add_item new_item s)
          |> fun s -> Some s)
    | Expand (i,_S) -> Some(expand (i,_S) s)  
    | Raw itm ->
      match itm.bs with
      | [] -> 
        Some (add_item (Cut_complete (itm.i,NT itm.nt,itm.k)) s)
      | _S::bs -> 
        s 
        |> add_item (Expand (itm.k,_S)) 
        |> add_item (Cut_blocked itm) 
        |> fun s -> Some s

let _ = step

let rec loop step s = 
  match step s with
  | None -> s
  | Some nxt -> loop step nxt


(* example data structures --------------------------------- *)


module Item_set = Tjr_set.Make(struct type t = item let compare: t -> t -> int = Pervasives.compare end)

let item_set_ops = Item_set.set_ops

let item_set_to_list = Item_set.Set_.elements


module Raw_set = Tjr_set.Make(struct type t = raw_item let compare: t -> t -> int = Pervasives.compare end)

let raw_set_ops = Raw_set.set_ops


let raw_set_to_list = Raw_set.Set_.elements


module Complete_map = Tjr_map.Make(struct type t = int * symbol let compare: t -> t -> int = Pervasives.compare end)

let complete_ops = Complete_map.map_ops


module Blocked_map = Tjr_map.Make(struct type t = int * symbol let compare: t -> t -> int = Pervasives.compare end)

let blocked_ops = Blocked_map.map_ops



module Int_set = Tjr_set.Make(struct type t = int let compare: t -> t -> int = Pervasives.compare end)
let int_set_ops = Int_set.set_ops


let int_set_to_list = Int_set.Set_.elements


let add_item itm s = 
  match item_set_ops.mem itm s.todo_done with
  | true -> s
  | false -> 
    {s with 
     todo_done=(item_set_ops.add itm s.todo_done);
     todo=(itm::s.todo)}
    

let cut itm j = 
  match itm.bs with
  | _S::bs -> { itm with as_=_S::itm.as_; bs; k=j }
  | _ -> failwith ""


(* specialize *)

let step = 
  step
    ~item_set_ops 
    ~raw_set_ops 
    ~raw_set_to_list 
    ~complete_ops
    ~blocked_ops 
    ~int_set_ops
    ~int_set_to_list
    ~add_item 
    ~cut 



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
    

let run_parser ~input =
  (* NOTE following is dependent on the grammar *)
  let expand (i,_S) s = 
    match _S with
    | TM _T -> expand_tm ~input (i,_T) s
    | NT _X -> expand_nt (i,_X) s
  in
  let step = step ~expand in
  let itm = Expand(0,NT _E) in
  let s = { 
    todo_done=(item_set_ops.add itm (item_set_ops.empty()));
    todo=[itm];
    complete=complete_ops.map_empty;
    blocked=blocked_ops.map_empty
  }
  in
  loop step s


let extract_results s = s.todo_done |> item_set_to_list 

let results_to_string rs = 
  rs |> results_to_yojson |> Yojson.Safe.pretty_to_string 

;;

let r = 
  run_parser ~input:"111" 
  |> extract_results 
  |> results_to_string 
  |> fun rs ->
  print_endline rs;
  Tjr_file.write_string_to_file ~fn:"results.json" rs

