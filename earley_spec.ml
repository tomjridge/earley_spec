open Tjr_set
open Tjr_map

module type S = sig
  type nonterm
  type terminal
end

module Make(S:S) = struct
  open S
  type symbol = NT of nonterm | TM of terminal

  type raw_item = { nt:nonterm; i:int; as_:symbol list; k:int; bs:symbol list }

  type item = 
    | Cut_complete of int * symbol * int
    | Cut_blocked of raw_item (* bs <> [] *)
    | Expand of int * symbol
    | Raw of raw_item

  (* complete and blocked maps are from k,S *)
  type 'a map_int_symbol

  type ('item_set,'raw_item_set,'int_set) state = {
    todo_done: 'item_set;
    todo: item list;
    blocked: 'raw_item_set map_int_symbol;
    complete: 'int_set map_int_symbol  (* map k,S -> j set *)
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

end



(* example ---------------------------------------------------------- *)

include Make(struct 
    type nonterm = int 
    type terminal = int 
  end)


module Item_set = Tjr_set.Make(struct type t = item let compare: t -> t -> int = Pervasives.compare end)

let item_set_ops = Item_set.set_ops


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


let expand (k,_S) s = 
  match _S with
  | TM tm -> failwith "FIXME"
  | NT nt -> failwith "FIXME"
