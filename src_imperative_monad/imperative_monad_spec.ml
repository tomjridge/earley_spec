module type M_ = sig
  type 'a m
  val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m
  val return: 'a -> 'a m
end

module type S_ = sig  


  (* FIXME don't have to be so abstract for specification - use
     concrete implementations of these *)
  type i_t = int  
  type k_t = int
  type j_t = int

  type nt
  type tm
  type sym
  val sym_case: nt:(nt -> 'a) -> tm:(tm -> 'a) -> sym -> 'a
  val _NT: nt -> sym

  type nt_item  

  val dot_nt: nt_item -> nt
  val dot_i: nt_item -> i_t
  val dot_k: nt_item -> k_t
  val dot_bs: nt_item -> sym list

  val cut: nt_item -> j_t -> nt_item

  type nt_item_set
  val elements : nt_item_set -> nt_item list
end

module Make(M:M_)(S:S_) = struct

  open M
  open S

  type atomic_operations = {
    get_items: unit -> nt_item list m;
    (* get_blocked_items: int * nt -> nt_item list m; *)
    add_items: nt_item list -> unit m;
  }

  let is_complete itm = (itm|>dot_bs = [])

  (* We construct sets of Earley items indexed by nat; this step
     function takes a set S_n and produces the set S_{n+1} *)
  let step ~at_ops ~expand_nt ~expand_tm ~input ~input_length ~get_blocked_items = 
    let { get_items; add_items } = at_ops in
    get_items () >>= fun previous_itms ->
    let rec loop itms = 
      match itms with 
      | [] -> return ()
      | itm::rest ->
        match itm|>dot_bs with
        | [] -> 
          (* item is complete *)
          let (k',_Y,k) = (itm|>dot_i, itm|>dot_nt, itm|>dot_k) in
          (* NOTE in order to be clear about the stages, we need to
             get blocked items from the previous stage; this ensures
             that the processing of each item at this stage does not
             interfere with each other - the loop could be
             "parallel" *)
          get_blocked_items (k',_Y) previous_itms |> fun bitms ->
          bitms |> List.map (fun bitm -> cut bitm k) |> fun itms ->
          add_items itms >>= fun _ ->
          loop rest
        | _S::bs ->
          (* item is not complete; deal with symbol S *)
          let bitm = itm in
          let k = bitm|>dot_k in
          _S |> sym_case
            ~nt:(fun _Y -> 
                expand_nt ~k ~nt:_Y |> fun itms ->
                add_items itms >>= fun _ ->
                loop rest)
            ~tm:(fun _T ->
                expand_tm ~k ~tm:_T ~input ~input_length |> fun js ->
                js |> List.map (fun j -> cut bitm j) |> fun itms ->
                add_items itms >>= fun _ ->
                loop rest)
    in
    loop previous_itms

  (* NOTE the mathematical spec of the above as sets indexed by nat would look better *)
                
                
          

end
