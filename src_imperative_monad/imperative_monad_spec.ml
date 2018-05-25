module type M_ = sig
  type 'a m
  val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m
  val return: 'a -> 'a m
end

module type S_ = sig
  type nt
  type tm
  type sym
  val sym_case: nt:(nt -> 'a) -> tm:(tm -> 'a) -> sym -> 'a

  type nt_item = { nt:nt; (* as *) i:int; k:int; bs:sym list }
  val cut: nt_item -> int -> nt_item
end

module Make(M:M_)(S:S_) = struct

  open M
  open S

  type atomic_operations = {
    get_items: unit -> nt_item list m;
    (* get_blocked_items: int * nt -> nt_item list m; *)
    add_items: nt_item list -> unit m;
  }

  let is_complete itm = (itm.bs = [])

  (* We construct sets of Earley items indexed by nat; this step
     function takes a set S_n and produces the set S_{n+1} *)
  let earley ~at_ops ~expand_nt ~expand_tm ~input ~get_blocked_items = 
    let { get_items; add_items } = at_ops in
    let rec step_set_of_states () = 
      get_items () >>= fun previous_itms ->
      let rec loop itms = 
        match itms with 
        | [] -> return ()
        | itm::rest ->
          match itm.bs with
          | [] -> 
            (* item is complete *)
            let (k',_Y,k) = (itm.i, itm.nt, itm.k) in
            (* NOTE in order to be clear about the stages, we need to
               get blocked items from the previous stage; this ensures
               that the processing of each item at this stage does not
               interfere with each other - the loop over the items could
               be "parallel" *)
            get_blocked_items (k',_Y) previous_itms |> fun bitms ->
            bitms |> List.map (fun bitm -> cut bitm k) |> fun itms ->
            add_items itms >>= fun _ ->
            loop rest
          | _S::bs ->
            (* item is not complete; deal with symbol S *)
            let bitm = itm in
            let k = bitm.k in
            _S |> sym_case
              ~nt:(fun _Y -> 
                  add_items (expand_nt ~k ~nt:_Y) >>= fun _ ->
                  loop rest)
              ~tm:(fun _T ->
                  expand_tm ~k ~tm:_T ~input |> fun js ->
                  js |> List.map (fun j -> cut bitm j) |> fun itms ->
                  add_items itms >>= fun _ ->
                  loop rest)
      in
      loop previous_itms
    in
    (* repeatedly apply *)

  (* NOTE the mathematical spec of the above as sets indexed by nat would look better *)
      
             
end
