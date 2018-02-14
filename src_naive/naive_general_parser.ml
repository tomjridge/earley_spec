
(* repeat till None, then return last state *)
let rec iter_opt f s =
  match f s with
  | None -> s
  | Some s -> iter_opt f s


(* types, prelude --------------------------------------------------- *)


type nt = int  [@@deriving yojson]
type tm = int  [@@deriving yojson]
type sym = NT of nt | TM of tm  [@@deriving yojson]


type nt_item = {
  nt: nt;
  i: int;
  as_: sym list;
  k: int;
  bs: sym list
}  [@@deriving yojson]

type results = nt_item list  [@@deriving yojson]


(* try to keep string_t separate from string *)
type string_t
type substring_t = (string_t * int * int)

let string_to_string_t: string -> string_t = (fun s -> Obj.magic s)
let string_t_to_string: string_t -> string = (fun s -> Obj.magic s)


type grammar_t = {
  nt_items_for_nt: nt -> (string_t * int) -> nt_item list;
  p_of_tm: tm -> substring_t -> int list
}

type input_t = {
  str: string_t;
  len: int;
}

type context = {
  grammar: grammar_t;
  input: input_t
}


let cut: nt_item -> int -> nt_item = 
  fun bitm j0 -> 
    let as_ = (List.hd bitm.bs)::bitm.as_ in
    let bs = List.tl bitm.bs in
    let k = j0 in
    let nitm ={bitm with k;as_;bs} in
    nitm 


type blocked_item = nt_item  (* bs <> [] *)

type key = (int * sym)

let bitm_to_key: blocked_item -> key = fun bitm -> (bitm.k,List.hd bitm.bs)

let comp = Pervasives.compare

module Nt_item_set = 
  Set.Make(struct type t = nt_item;; let compare: t -> t -> int = comp end)



(* code ------------------------------------------------------------- *)


type state = Nt_item_set.t


let state_to_bitms: state -> key -> blocked_item list = 
  fun s0 key -> 
    (Nt_item_set.elements s0)
    |> List.map (function
        | nitm when (nitm.bs <> [] && bitm_to_key nitm = key) -> [nitm]
        | _ -> [])
    |> List.concat


let new_items : context -> state -> nt_item -> nt_item list = 
  fun c0 s0 nitm -> 
    let complete = (nitm.bs = []) in
    match complete with
    | true -> 
      let (k,sym,j) = (nitm.i,NT(nitm.nt),nitm.k) in
      (* let citm : citm_t = {k;sym;j} in *)
      let key = (k,sym) in
      let bitms = state_to_bitms s0 key in
      let f bitm = cut bitm j in
      List.map f bitms
    | false -> 
      (* blocked, so process next sym *)
      let bitm = nitm in
      let (k,sym) = (bitm.k,List.hd nitm.bs) in
      (* now look at symbol we are blocked on *)
      match sym with
      | NT nt -> 
        let nitms = c0.grammar.nt_items_for_nt nt (c0.input.str,k) in
        nitms
      | TM tm -> 
        (* parse tm and complete with item *)
        let k = bitm.k in
        let p = c0.grammar.p_of_tm tm in
        let js = p (c0.input.str,k,c0.input.len) in
        let f j = cut bitm j in
        List.map f js


(* FIXME very inefficient; could at least separate into items that
   have definitely been processed and those pending *)
let step c0 = 
  let f s = 
    let new_itms = 
      Nt_item_set.elements s
      |> List.map (new_items c0 s)
      |> List.concat
      |> Nt_item_set.of_list
    in
    let s1 = Nt_item_set.union s new_itms in
    (* NOTE this works because s0 <= s1 *)
    let set_equal xs ys = Nt_item_set.(cardinal xs = cardinal ys) in
    if set_equal s1 s then None else Some s1
  in
  fun s -> iter_opt f s


(* construct initial context, apply step *)
let run : context -> nt -> Nt_item_set.t = 
  fun c0 nt ->
    {nt;i=0;as_=[];k=0;bs=[NT nt]} |> fun init ->
    Nt_item_set.of_list [init] 
    |> step c0 
    |> Nt_item_set.remove init  (* remove the dummy item; assume no X -> X rule *)


(* post-processing -------------------------------------------------- *)

let extract_results r = r |> Nt_item_set.elements

let results_to_string xs = 
  xs 
  |> results_to_yojson 
  |> Yojson.Safe.pretty_to_string
