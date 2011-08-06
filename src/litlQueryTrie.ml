(************************************************************************
*
*  litlQueryTrie.ml
*  
*
*  Created by Olivier Brunet on 29 Mar 2011.
*  Copyright (c) 2011 B.W.C. Computing. All rights reserved.
*
************************************************************************)

(*
#use "topfind" ;;
#require "litlCore" ;;
#require "litlLib" ;;

*)

(*

module I = struct type t = int let compare = compare let equal = (=) end
module M = Litl.Map (I) ;;

*)

module Make (M : MAP) = struct
  module Trie = Litl.Trie (M)

  type elt = Trie.elt
  type t = {
    trie : Trie.t ;
    list : Trie.t list
  }

  exception Eject

  (* val empty : t *)
  let empty = { trie = Trie.empty ; list = [] }

  (* val is_empty : t -> bool *)
  let is_empty t = Trie.is_empty t.trie

  (* val height : t -> int *)
  let height t = Trie.height t.trie

  (* val zoom : t -> elt zoom *)
  let zoom t = Trie.zoom t.trie

  (* val mem : elt -> t -> bool *)
  let mem elt t = Trie.mem elt t.trie

  (* val to_trie : t -> trie *)
  let to_trie t = t.trie

  (* val from_trie : trie -> t *)
  let from_trie trie = begin 
    let rec aux key trie queue list = begin 
      let (current, list') = begin 
        match list with
          current :: list' -> (current, list')
        | [] -> (Trie.empty, [])
      end in
      let current' = begin 
        Trie.add_next LitlDeque.next (LitlDeque.cons key queue) current
	    end
	    and queue' = LitlDeque.cons_right queue key in
	    current' :: (
		    M.fold (
			    fun key' trie' list'' -> aux key' trie' queue' list''
			  ) trie.Trie.next list'
		  )
    end in
    let floors = M.fold (
	    fun key trie list -> aux key trie LitlDeque.empty list
	  ) trie.Trie.next [] in
	  { trie = trie ; list = floors }
  end

  (* val do_true_down :
    trie -> trie list -> (atom * trie * atom queue * trie) list -> t *)
  let rec do_true_down trie floors stack = begin 
  	match stack with 
      [] -> { trie = trie ; list = floors }
    | (key, trie', _, floor) :: stack' -> begin 
  	    let new_trie = { trie' with 
  	      Trie.next = M.set key trie trie'.Trie.next
  	    } in
  	    do_true_down new_trie (floor :: floors) stack'
    end
  end

	(* val do_true_up :
	  elt -> atom deque -> trie list ->
	   (atom * trie * atom deque * trie) list -> t *)
  let rec do_true_up keys trie queue floors stack = begin 
    match keys with 
    | [] -> begin 
      let new_trie = { Trie.present = true ; Trie.next = M.empty } 
      in
      do_true_down new_trie floors stack
    end
    | key :: keys' -> begin 
      let queue' = LitlDeque.cons_right queue key in
      match floors with
      | [] -> begin 
	      let floor = 
	        Trie.add_next LitlDeque.next (LitlDeque.cons key queue) Trie.empty
	      in
	      (* The queue in the stack will not be used. *)
	      let stack' = (key, trie, LitlDeque.empty, floor) :: stack in
        do_true_up keys' Trie.empty queue' [] stack'
      end
      | floor :: floors' -> begin 
	      let floor' = 
	        Trie.add_next LitlDeque.next (LitlDeque.cons key queue) floor
	      in
	      (* The queue in the stack will not be used. *)
	      let stack' = (key, trie, LitlDeque.empty, floor') :: stack in
        do_true_up keys' Trie.empty queue' floors' stack'
      end
    end
  end

  (* val do_false :
	  trie -> trie list ->
	  (atom * trie * atom deque * trie) list -> t *)
  let rec do_false trie floors stack = begin 
    match stack with
      [] -> { trie = trie ; list = floors }
    | (key, trie', queue, floor) :: stack' -> begin 
      if Trie.is_empty trie then begin 
        let floor' =
          Trie.remove_next LitlDeque.next (LitlDeque.cons key queue) floor
        and new_trie = { trie' with
	        Trie.next = M.remove key trie'.Trie.next
	      } in
	      do_false new_trie (floor' :: floors) stack'
      end
      else begin 
        let new_trie = { trie' with
          Trie.next = M.set key trie trie'.Trie.next
        } in
        do_false new_trie (floor :: floors) stack'
      end
    end
  end

  (* val change_aux :
    (bool -> bool) -> elt -> trie ->
    atom queue -> trie list ->
    (atom * trie * atom queue * trie) list -> t *)
  let rec change_aux f keys trie queue floors stack = begin 
    match keys with
    | [] -> begin 
      let when_true = f true in
      if when_true = trie.Trie.present
      then (* There is nothing to do *)
        raise Eject
      else
	      if when_true 
        then (* when_true is true, so that we do add the element *)
          do_true_down {trie with Trie.present = true} floors stack
        else (* when_true is false, so that we remove the element *)
          do_false {trie with Trie.present = false} floors stack
    end
    | key :: keys' -> begin 
      match M.find_opt key trie.Trie.next with
      | None -> begin 
        if f false
        then (* We must add this element, since it's not present *)
          do_true_up keys trie queue floors stack
	      else (* we want to remove an element that's not present *)
		      raise Eject
		  end
		  | Some trie' -> begin 
        match floors with 
          [] -> assert false
        | floor :: floors' -> 
	        let queue' = LitlDeque.cons_right queue key
	        and stack' = (key, trie, queue, floor) :: stack in
	        change_aux f keys' trie' queue' floors' stack'
	    end
    end
  end

  (* let change f keys t = begin 
    change_aux f keys t.trie LitlDeque.empty t.list []
  end *)

  (* val change : (bool -> bool) -> elt -> t -> t *)
  let change f keys t = begin 
    try
      change_aux f keys t.trie LitlDeque.empty t.list []
    with Eject -> t
  end

  (* val change_return : (bool -> bool * 'a) -> elt -> t -> t * 'a *)
  let change_return f keys t = begin 
    let result = ref None in
    let f' b = begin 
      let (b', res) = f b in
      result := Some res ; b'
    end in
    let t' = change f' keys t in
    match !result with
      None -> assert false
    | Some res -> (t', res)
  end

  (* val add : elt -> t -> t *)
	let add keys t = begin 
	  change (fun _ -> true) keys t
	end

  (* val remove : elt -> t -> t *)
	let remove keys t = begin 
	  change (fun _ -> false) keys t
	end

  (* val singleton : elt -> t *)
  let singleton keys = begin 
    change (fun _ -> true) keys empty
  end

  (* val do_true_up_next :
	  ('a -> (atom * 'a) option) -> 'a ->
	  trie -> atom queue -> trie list ->
    (atom * trie * atom queue * trie) list -> t *)
  let rec do_true_up_next nexter keys trie queue floors stack = begin 
    match nexter keys with 
    | None -> begin 
      let new_trie = { Trie.present = true ; Trie.next = M.empty } 
      in
      do_true_down new_trie floors stack
    end
    | Some (key, keys') -> begin 
      let queue' = LitlDeque.cons_right queue key in
      match floors with
      | [] -> begin 
	      let floor = 
	        Trie.add_next LitlDeque.next (LitlDeque.cons key queue) Trie.empty
	      in
	      (* The queue in the stack will not be used. *)
	      let stack' = (key, trie, LitlDeque.empty, floor) :: stack in
        do_true_up_next nexter keys' Trie.empty queue' [] stack'
      end
      | floor :: floors' -> begin 
	      let floor' = 
	        Trie.add_next LitlDeque.next (LitlDeque.cons key queue) floor
	      in
	      (* The queue in the stack will not be used. *)
	      let stack' = (key, trie, LitlDeque.empty, floor') :: stack in
        do_true_up_next nexter keys' Trie.empty queue' floors' stack'
      end
    end
  end

	(* val change_aux_next :
	  (bool -> bool) ->
	  ('a -> (atom * 'a) option) -> 'a ->
	  trie -> atom queue -> trie list ->
	  (atom * trie * atom queue * trie) list -> t *)
  let rec change_aux_next f nexter keys trie queue floors stack = begin 
    match nexter keys with
    | None -> begin 
      let when_true = f true in
      if when_true = trie.Trie.present
      then (* There is nothing to do *)
        raise Eject
      else
	      if when_true 
        then (* when_true is true, so that we do add the element *)
          do_true_down {trie with Trie.present = true} floors stack
        else (* when_true is false, so that we remove the element *)
          do_false {trie with Trie.present = false} floors stack
    end
    | Some (key, keys') -> begin 
      match M.find_opt key trie.Trie.next with
      | None -> begin 
        if f false
        then (* We must add this element, since it's not present *)
          do_true_up_next nexter keys trie queue floors stack
	      else (* we want to remove an element that's not present *)
		      raise Eject
		  end
		  | Some trie' -> begin 
        match floors with 
          [] -> assert false
        | floor :: floors' -> 
	        let queue' = LitlDeque.cons_right queue key
	        and stack' = (key, trie, queue, floor) :: stack in
	        change_aux_next f nexter keys' trie' queue' floors' stack'
	    end
    end
  end

  (* val change_next :
	  (bool -> bool) -> ('a -> (atom * 'a) option) -> 'a -> t -> t *)
  let change_next f nexter keys t = begin 
    try
      change_aux_next f nexter keys t.trie LitlDeque.empty t.list []
    with Eject -> t
  end

  (* val change_return_next :
	  (bool -> bool * 'a) ->
	  ('b -> (atom * 'b) option) -> 'b -> t -> t * 'a *)
  let change_return_next f nexter keys t = begin 
    let result = ref None in
    let f' b = begin 
      let (b', res) = f b in
      result := Some res ; b'
    end in
    let t' = change_next f' nexter keys t in
    match !result with
      None -> assert false
    | Some res -> (t', res)
  end
  
  (* val add_next : ('a -> (atom * 'a) option) -> 'a -> t -> t *)
	let add_next nexter keys t = begin 
	  change_next (fun _ -> true) nexter keys t
	end

  (* val remove_next : ('a -> (atom * 'a) option) -> 'a -> t -> t *)	
	let remove_next nexter keys t = begin 
	  change_next (fun _ -> false) nexter keys t
	end

  (* val singleton_next : ('a -> (atom * 'a) option) -> 'a -> t *)
  let singleton_next nexter keys = begin 
    change_next (fun _ -> true) nexter keys empty
  end
  
  (* val union : t -> t -> t *)
  let union t1 t2 = begin 
    let rec aux l1 l2 lres = begin 
      match l1 with
        [] -> List.rev_append lres l2
      | tl1 :: l1' -> begin 
        match l2 with
          [] -> List.rev_append lres l1
        | tl2 :: l2' -> aux l1' l2' ((Trie.union tl1 tl2) :: lres)
	    end
	  end in
    let new_trie = Trie.union t1.trie t2.trie in
    { trie = new_trie ;
      list = aux t1.list t2.list []
    }
  end

  (* val inter : t -> t -> t *)
  let inter t1 t2 = begin 
    from_trie (Trie.inter t1.trie t2.trie)
  end

  (* val diff : t -> t -> t *)
  let diff t1 t2 = begin 
    from_trie (Trie.diff t1.trie t2.trie)
  end

  (* val subset : t -> t -> bool *)
  let subset t1 t2 = begin 
    Trie.subset t1.trie t2.trie
  end
    
  (* val enum : t -> elt enum *)
  let enum t = begin 
    Trie.enum t.trie
  end

  (* val iter : (elt -> unit) -> t -> unit *)
  let iter f t = begin 
    Trie.iter f t.trie
  end

  (* val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a *)
  let fold f t accu = begin 
    Trie.fold f t.trie accu
  end

  (* val for_all : (elt -> bool) -> t -> bool *)
  let for_all f t = begin 
    Trie.for_all f t.trie
  end  

  (* val exists : (elt -> bool) -> t -> bool *)
  let exists f t = begin 
    Trie.exists f t.trie
  end

  (* val compare : t -> t -> int *)
  let compare t1 t2 = begin 
    Trie.compare t1.trie t2.trie
  end

  (* val equal : t -> t -> bool *)
  let equal t1 t2 = begin 
    Trie.equal t1.trie t2.trie
  end

  (* val choose : t -> elt *)
  let choose t = begin 
    Trie.choose t.trie
  end

  (* val min : t -> elt *)
  let min t = begin 
    Trie.min t.trie
  end

  (* val max : t -> Trie.elt *)
  let max t = begin 
    Trie.max t.trie
  end

  (* val split : elt -> t -> t * bool * t *)  
  let split keys t = begin 
    let (trie1, res, trie2) = Trie.split keys t.trie in
    (from_trie trie1, res, from_trie trie2)
  end

  (* val split_left : elt -> t -> t * bool *)
  let split_left keys t = begin 
    let (trie1, res) = Trie.split_left keys t.trie in
    (from_trie trie1, res)
  end

  (* val split_right : elt -> t -> bool * t *)
  let split_right keys t = begin 
    let (res, trie2) = Trie.split_right keys t.trie in
    (res, from_trie trie2)
  end

  (* val filter : (elt -> bool) -> t -> t *)
  let filter f t = begin 
    Trie.fold (fun elt t -> if f elt then add elt t else t) t.trie empty
  end
end

(*

let add keys t = change (fun _ -> true) keys t ;;
let remove keys t = change (fun _ -> false) keys t ;;

let f t = LitlEnum.to_list (Trie.enum t) ;;

let tt = empty ;;
let tt = add [1;2;3] tt ;;
let tt = add [1;3;4] tt ;;
let tt = add [1;3] tt ;;
let tt = add [2;3] tt ;;

let t = to_trie tt ;;
let ttt = from_trie t ;;


List.map f tt.list ;;

LitlEnum.to_list (Trie.enum tt.trie) ;;

let tt = remove [1;3] tt ;;
let tt = remove [1;2] tt ;;
let tt = remove [1;2;3] tt ;;
let tt = remove [1;3;4] tt ;;

*)