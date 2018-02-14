
let dest_Some = function Some x -> x | _ -> (failwith "dest_Some")

let find_with_default d f k m = try (f k m) with Not_found -> d

let rec while_not_nil': 'a list -> 's -> ('s -> 'a -> 's) -> 's = (
    fun xs s0 f -> List.fold_left f s0 xs)

let rec while_not_nil: 'a list -> ('a -> 's -> 's) -> 's -> 's = (
    fun xs f s0 -> List.fold_left (fun x y -> f y x) s0 xs)

type k_t = int
type i_t = int
type j_t = int

type nt = int
type tm = int
type sym = NT of nt | TM of tm


type nt_item = {
    nt: nt;
    i: i_t;
    as_: sym list;
    k: k_t;
    bs: sym list
  }

type tm_item = {
    k: k_t;
    tm: tm
  }

type sym_item = {
    k: k_t;
    sym: sym
  }

type citm_t = {
    k: k_t;
    sym: sym;
    j: j_t 
  }

type string_t
type substring_t = (string_t * i_t * j_t)

let string_to_string_t: string -> string_t = (fun s -> Obj.magic s)
let string_t_to_string: string_t -> string = (fun s -> Obj.magic s)


type grammar_t = {
    nt_items_for_nt: nt -> (string_t * int) -> nt_item list;
    p_of_tm: tm -> substring_t -> k_t list
  }

type input_t = {
    str: string_t;
    len: int;
  }

type ctxt_t = {
    g0: grammar_t;
    i0: input_t
  }



let cut: nt_item -> j_t -> nt_item = (
    fun bitm j0 -> (
      let as_ = (List.hd bitm.bs)::bitm.as_ in
      let bs = List.tl bitm.bs in
      let k = j0 in
      let nitm ={bitm with k;as_;bs} in
      nitm ))

module Unstaged = struct

  type bitm_t = nt_item  (* bs <> [] *)

  type b_key_t = (k_t * sym)

  let bitm_to_key: bitm_t -> b_key_t = (
      fun bitm -> (bitm.k,List.hd bitm.bs))

  type c_key_t = (k_t * sym)
               
  let citm_to_key: citm_t -> c_key_t = (
      fun citm -> (citm.k,citm.sym))

end

let comp = Pervasives.compare

module Int_set = 
  Set.Make(struct type t = int;; let compare: t -> t -> int = comp end)

module Nt_item_set = 
  Set.Make(struct type t = nt_item;; let compare: t -> t -> int = comp end)

module Sym_item_set = 
  Set.Make(struct type t = sym_item;; let compare: t -> t -> int = comp end)

module Nt_set = 
  Set.Make(struct type t = nt;; let compare: t -> t -> int = comp end)

module Map_tm =
  Map.Make(struct type t = tm;; let compare: t -> t -> int = comp end)

module Map_nt =
  Map.Make(struct type t = nt;; let compare: t -> t -> int = comp end)

module Map_int = 
  Map.Make(struct type t = int;; let compare: t -> t -> int = comp end)


let debug = ref false

let debug_endline = (
    fun x -> 
    if !debug then print_endline x else ())

let sym_to_string s = (match s with | NT nt -> Printf.sprintf "NT %d" nt | TM tm -> Printf.sprintf "TM %d" tm)

let sym_list_to_string ss = (ss |> List.map sym_to_string |> String.concat "," |> fun x -> "["^x^"]")

let nitm_to_string nitm = (
    Printf.sprintf "(%d %d %s %d %s)" 
                   nitm.nt nitm.i 
                   (sym_list_to_string nitm.as_) 
                   nitm.k 
                   (sym_list_to_string nitm.bs))



(* --------------------------------------------------------------------- *)

open Unstaged

type spec_item_t = nt_item

module Spec_t = struct 
  include Nt_item_set
      
  (* for < 4.02.0 *)
  let of_list: elt list -> t = (
    fun xs -> 
      List.fold_left (fun a b -> add b a) empty xs
  )

end

type spec_t = Spec_t.t

let spec_to_bitms: spec_t -> b_key_t -> bitm_t list = (
  fun s0 key -> 
    (Spec_t.elements s0)
    |> List.map (function
        | nitm when (nitm.bs <> [] && bitm_to_key nitm = key) -> [nitm]
        | _ -> [])
    |> List.concat)

let new_items : ctxt_t -> spec_t -> spec_item_t -> spec_item_t list = (
  fun c0 s0 nitm -> (
      let complete = (nitm.bs = []) in
      match complete with
      | true -> (
          let (k,sym,j) = (nitm.i,NT(nitm.nt),nitm.k) in
          (* let citm : citm_t = {k;sym;j} in *)
          let key = (k,sym) in
          let bitms = spec_to_bitms s0 key in
          let f bitm = cut bitm j in
          List.map f bitms)
      | false -> (
          (* blocked, so process next sym *)
          let bitm = nitm in
          let (k,sym) = (bitm.k,List.hd nitm.bs) in
          (* now look at symbol we are blocked on *)
          match sym with
          | NT nt -> (
              let nitms = c0.g0.nt_items_for_nt nt (c0.i0.str,k) in
              nitms)
          | TM tm -> (
              (* parse tm and complete with item *)
              let k = bitm.k in
              let p = c0.g0.p_of_tm tm in
              let js = p (c0.i0.str,k,c0.i0.len) in
              let f j = cut bitm j in
              List.map f js)
        )        
    )
)


let rec spec' c0 s0 = (
  let new_itms = (
    (Spec_t.elements s0)
    |> List.map (new_items c0 s0)
    |> List.concat
    |> Spec_t.of_list)
  in
  let s1 = Spec_t.union s0 new_itms in
  if Spec_t.equal s1 s0 then s0 else spec' c0 s1)


(* construct initial context, apply spec' *)
let se_spec : ctxt_t -> nt -> Nt_item_set.t = (
  fun c0 nt ->
    let init = {nt;i=0;as_=[];k=0;bs=[NT nt]} in
    let s0 = Spec_t.of_list [init] in
    let s1 = spec' c0 s0 in
    Spec_t.remove init s1)  (* remove the dummy item *)
