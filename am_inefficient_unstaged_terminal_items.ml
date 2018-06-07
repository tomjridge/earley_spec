#require "ppx_deriving_yojson";;

(* repeat till None, then return last state *)
let rec iter_opt f s =
  match f s with
  | None -> s
  | Some s -> iter_opt f s


(* types, prelude --------------------------------------------------- *)


type nt = int  [@@deriving yojson]
type tm = int  [@@deriving yojson]  (* matches exactly that char *)
type sym = NT of nt | TM of tm  [@@deriving yojson]


type item = {
  sym: sym;
  i: int;
  as_: sym list;
  k: int;
  bs: sym list
}  [@@deriving yojson]


(* some pretty printing *)

let itm_to_string ~sym_to_string itm =
  Printf.sprintf 
    "(%s -> %d [%s] %d [%s])"
    (itm.sym |> sym_to_string)
    itm.i
    (itm.as_ |> List.map sym_to_string |> String.concat ",")
    itm.k
    (itm.bs |> List.map sym_to_string |> String.concat ",")

type results = item list  [@@deriving yojson]


type string_t = string

type grammar_t = {
  items_for_nt: nt -> (string_t * int) -> item list;
  p_of_tm: tm -> string_t -> int -> int list
}

type input_t = {
  str: string_t;
  len: int;
}

type context = {
  grammar: grammar_t;
  input: input_t
}


let cut: item -> int -> item = 
  fun bitm j0 -> 
    let as_ = (List.hd bitm.bs)::bitm.as_ in
    let bs = List.tl bitm.bs in
    let k = j0 in
    let nitm ={bitm with k;as_;bs} in
    nitm 


type blocked_item = item  (* bs <> []; invariant: sym is a nt *)

type key = (int * sym)  (* NOTE sym can be terminal *)

let bitm_to_key: blocked_item -> key = fun bitm -> (bitm.k,List.hd bitm.bs)

let comp = Pervasives.compare

module Item_set = 
  Set.Make(struct type t = item;; let compare: t -> t -> int = comp end)



(* code ------------------------------------------------------------- *)


type state = Item_set.t


let state_to_bitms: state -> key -> blocked_item list = 
  fun s0 key -> 
    (Item_set.elements s0)
    |> List.map (function
        | nitm when (nitm.bs <> [] && bitm_to_key nitm = key) -> [nitm]
        | _ -> [])
    |> List.concat

(* NOTE this is a nice formal spec for what happens in Earley parsing *)
let new_items : context -> state -> item -> item list = 
  fun c0 s0 nitm -> 
    let complete = (nitm.bs = []) in
    match complete with
    | true -> 
      let (k,sym,j) = (nitm.i,nitm.sym,nitm.k) in
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
      | NT nt -> c0.grammar.items_for_nt nt (c0.input.str,k)
      | TM tm ->         
        let k = bitm.k in
        let p = c0.grammar.p_of_tm tm in
        let js = p c0.input.str k in
        List.map (fun j -> {sym=TM tm; i=k; as_=[]; k=j; bs=[] }) js  (* terminal tm items; could wrap *)


(* FIXME very inefficient; could at least separate into items that
   have definitely been processed and those pending *)
let step c0 = 
  let f s = 
    let new_itms = 
      Item_set.elements s
      |> List.map (new_items c0 s)
      |> List.concat
      |> Item_set.of_list
    in
    let s1 = Item_set.union s new_itms in
    (* NOTE this works because s0 <= s1 *)
    let set_equal xs ys = Item_set.(cardinal xs = cardinal ys) in
    if set_equal s1 s then None else Some s1
  in
  fun s -> iter_opt f s


(* construct initial context, apply step *)

let special_sym = NT (-1)

let run : context -> nt -> Item_set.t = 
  fun c0 nt ->
    {sym=special_sym;i=0;as_=[];k=0;bs=[NT nt]} |> fun init ->
    Item_set.of_list [init] 
    |> step c0 
    |> Item_set.remove init  (* remove the dummy item; assume no X -> X rule *)


(* post-processing -------------------------------------------------- *)

let extract_results r = r |> Item_set.elements

let results_to_string xs = 
  xs 
  |> results_to_yojson 
  |> Yojson.Safe.pretty_to_string


(* example ---------------------------------------------------------- *)


(* various examples *)

let eps = TM 1
let x = TM 3

let parse_eps s i = 
  assert(i <= String.length s);
  [i]

let parse_x s i =
  (* this terminal parser requires to know string_t *)
  if i < String.length s && String.get s i = 'x' then 
    [i+1]
  else
    []

let _ = parse_x

let p_of_tm = 
  fun tm -> 
    if TM tm=eps then parse_eps
    else if TM tm=x then parse_x
    else failwith __LOC__

let i0 str = 
  let len = String.length str in
  { str; len }


(* E -> E E E | "x" | eps *)
let e' = 2
let e = NT e'

let items_for_nt = fun nt (s,i) ->
  let _ = assert(nt=e') in
  let sym = NT nt in
  let as_ = [] in
  let k = i in
  [{sym;i;as_;k;bs=[e;e;e]};
   {sym;i;as_;k;bs=[x]};
   {sym;i;as_;k;bs=[eps]}]

let grammar = {items_for_nt; p_of_tm}

let c0 str = {grammar;input=(i0 str)}

let sym_to_string = function
  | TM 1 -> "eps"
  | TM 3 -> "x"
  | NT 2 -> "E"
  | NT -1 -> "[special symbol]"
  | _ -> failwith __LOC__


let main () = 
  let len = 4 in 
  let str = String.make len 'x' in
  run (c0 str) e' 
  |> extract_results
  |> List.map (itm_to_string ~sym_to_string)
  |> List.iter print_endline



let _ = main ()


(*


([special symbol] -> 0 [E] 0 [])
([special symbol] -> 0 [E] 1 [])
([special symbol] -> 0 [E] 2 [])
([special symbol] -> 0 [E] 3 [])
([special symbol] -> 0 [E] 4 [])
(E -> 0 [] 0 [E,E,E])
(E -> 0 [] 0 [eps])
(E -> 0 [] 0 [x])
(E -> 0 [E] 0 [E,E])
(E -> 0 [E] 1 [E,E])
(E -> 0 [E] 2 [E,E])
(E -> 0 [E] 3 [E,E])
(E -> 0 [E] 4 [E,E])
(E -> 0 [E,E] 0 [E])
(E -> 0 [E,E] 1 [E])
(E -> 0 [E,E] 2 [E])
(E -> 0 [E,E] 3 [E])
(E -> 0 [E,E] 4 [E])
(E -> 0 [E,E,E] 0 [])
(E -> 0 [E,E,E] 1 [])
(E -> 0 [E,E,E] 2 [])
(E -> 0 [E,E,E] 3 [])
(E -> 0 [E,E,E] 4 [])
(E -> 0 [eps] 0 [])
(E -> 0 [x] 1 [])
(E -> 1 [] 1 [E,E,E])
(E -> 1 [] 1 [eps])
(E -> 1 [] 1 [x])
(E -> 1 [E] 1 [E,E])
(E -> 1 [E] 2 [E,E])
(E -> 1 [E] 3 [E,E])
(E -> 1 [E] 4 [E,E])
(E -> 1 [E,E] 1 [E])
(E -> 1 [E,E] 2 [E])
(E -> 1 [E,E] 3 [E])
(E -> 1 [E,E] 4 [E])
(E -> 1 [E,E,E] 1 [])
(E -> 1 [E,E,E] 2 [])
(E -> 1 [E,E,E] 3 [])
(E -> 1 [E,E,E] 4 [])
(E -> 1 [eps] 1 [])
(E -> 1 [x] 2 [])
(E -> 2 [] 2 [E,E,E])
(E -> 2 [] 2 [eps])
(E -> 2 [] 2 [x])
(E -> 2 [E] 2 [E,E])
(E -> 2 [E] 3 [E,E])
(E -> 2 [E] 4 [E,E])
(E -> 2 [E,E] 2 [E])
(E -> 2 [E,E] 3 [E])
(E -> 2 [E,E] 4 [E])
(E -> 2 [E,E,E] 2 [])
(E -> 2 [E,E,E] 3 [])
(E -> 2 [E,E,E] 4 [])
(E -> 2 [eps] 2 [])
(E -> 2 [x] 3 [])
(E -> 3 [] 3 [E,E,E])
(E -> 3 [] 3 [eps])
(E -> 3 [] 3 [x])
(E -> 3 [E] 3 [E,E])
(E -> 3 [E] 4 [E,E])
(E -> 3 [E,E] 3 [E])
(E -> 3 [E,E] 4 [E])
(E -> 3 [E,E,E] 3 [])
(E -> 3 [E,E,E] 4 [])
(E -> 3 [eps] 3 [])
(E -> 3 [x] 4 [])
(E -> 4 [] 4 [E,E,E])
(E -> 4 [] 4 [eps])
(E -> 4 [] 4 [x])
(E -> 4 [E] 4 [E,E])
(E -> 4 [E,E] 4 [E])
(E -> 4 [E,E,E] 4 [])
(E -> 4 [eps] 4 [])
(eps -> 0 [] 0 [])
(eps -> 1 [] 1 [])
(eps -> 2 [] 2 [])
(eps -> 3 [] 3 [])
(eps -> 4 [] 4 [])
(x -> 0 [] 1 [])
(x -> 1 [] 2 [])
(x -> 2 [] 3 [])
(x -> 3 [] 4 [])


*)
