#require "ppx_deriving_yojson";;

(* a version based on dynamic programming *)


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

type grammar_t = {
  expand_nt: nt:nt -> str:string -> index:int -> item list;
  expand_tm: tm:tm -> str:string -> index:int -> item list
}


type context = {
  grammar: grammar_t;
  input: string
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

(* we construct sets S(i,j) inductively *)

let _S = Array.make_matrix 100 100 Item_set.empty  (* we update this imperatively *)


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
    let str = c0.input in
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
      | NT nt -> c0.grammar.expand_nt ~nt ~str ~index:k
      | TM tm -> c0.grammar.expand_tm ~tm ~str ~index:k


let set_equal xs ys = Item_set.(cardinal xs = cardinal ys) 


(* NOTE this is a sort of double induction *)
(* FIXME call these sets _S(k)(i) so that k matches the itm.k *)
(* FIXME work with sets rather than converting to lists *)
let step ~ctxt _S = 
  let rec loop (i,k) = 
    let _S_i_k = _S.(i).(k) in
    match i > String.length ctxt.input with | true -> () | false ->
      (* update _S(i,k+1) *)
      let new_itms = 
        Item_set.elements _S_i_k
        |> List.map (new_items ctxt _S_i_k)
        |> List.concat
        |> Item_set.of_list
        |> Item_set.elements
      in
      (* FIXME some of these items have different k values if we allow arbitrary terminals *)
      let at_this_stage,at_future_stages = List.partition (fun itm -> itm.k = i) new_itms in
      at_future_stages |> List.iter (fun itm ->
          _S.(itm.k).(0) <- Item_set.add itm (_S.(itm.k).(0)));
      _S.(i).(k+1) <- Item_set.union _S_i_k (Item_set.of_list at_this_stage);
      if set_equal _S.(i).(k) _S.(i).(k+1) then loop (i+1,0) else loop (i,k+1)
  in
  loop (0,0)


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
  (* this terminal parser requires to know string *)
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
