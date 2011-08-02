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

(* module Enum = LitlEnum *)

module MutableList = struct 
  type 'a elt = Nil | Cons of 'a * 'a t
  and 'a t = 'a elt ref

  let cons elt (t : 'a t) = begin 
    (ref (Cons (elt, t)) : 'a t)
  end

  let enum t = begin 
    let aux t = begin 
      match !t with
        Nil -> None
      | Cons (a, b) -> Some (b, a)
    end in
    LitlEnum.from_generator aux t
  end
end

(*

module I = struct type t = int let compare = compare let equal = (=) end
module M = Litl.Map (I) ;;

*)

module Make (M : MAP) = struct
  module Trie = Litl.Trie (M)

  type t = {
    trie : Trie.t ;
    list : Trie.t list
  }

  exception Eject

  let empty = { trie = Trie.empty ; list = [] }

  let is_empty t = Trie.is_empty t.trie

  let height t = Trie.height t.trie

  let zoom t = Trie.zoom t.trie

  let mem elt t = Trie.mem elt t.trie

  (* val add_aux :
    Trie.atom MutableList.t ->
    Trie.atom MutableList.elt ref ->
    Trie.atom list -> Trie.t list -> Trie.t list -> Trie.t list *)
  (* let rec add_aux head tail elt do_ne to_do = begin 
    match elt with
      pre :: elt' -> begin 
        let (trie, to_do') = match to_do with
          [] -> (Trie.empty, [])
        | trie :: to_do' -> (trie, to_do') in 
        let trie' = begin 
          Trie.add_enum (MutableList.enum (MutableList.cons pre head)) trie
        end in
        let new_tail = ref MutableList.Nil in
        tail := MutableList.Cons (pre, new_tail) ;
        add_aux head new_tail elt' (trie' :: do_ne) to_do'
      end
    | _ -> List.rev_append do_ne to_do
  end *)

  (* val add_aux :
    M.index LitlDeque.t ->
        M.index list -> Trie.t list -> Trie.t list -> Trie.t list *)
  let rec add_aux queue elt do_ne to_do = begin 
    match elt with
      pre :: elt' -> begin 
        let (trie, to_do') = begin 
          match to_do with
            [] -> (Trie.empty, [])
          | trie :: to_do' -> (trie, to_do')
        end in
        let trie' = begin 
          Trie.add_next LitlDeque.next (LitlDeque.cons pre queue) trie
        end in
        add_aux (LitlDeque.cons_right queue pre) elt' (trie' :: do_ne) to_do'
      end
    | [] -> List.rev_append do_ne to_do
  end

  let add elt t = begin 
    let new_trie = Trie.add elt t.trie in
    let new_list = begin 
      match elt with
        [] -> t.list
      | pre :: elt' -> add_aux LitlDeque.empty elt' [] t.list
    end
    in {
      trie = new_trie ;
      list = new_list
    }
  end

  let rec add_next_aux nexter queue elt do_ne to_do = begin 
    match nexter elt with
      Some (pre, elt') -> begin 
        let (trie, to_do') = match to_do with
          [] -> (Trie.empty, [])
        | trie :: to_do' -> (trie, to_do') in 
        let trie' = begin 
          Trie.add_next LitlDeque.next (LitlDeque.cons pre queue) trie
        end in
        add_next_aux nexter (
          LitlDeque.cons_right queue pre
        ) elt' (trie' :: do_ne) to_do'
      end
    | _ -> List.rev_append do_ne to_do
  end

  (* val add_next : ('a -> (atom * 'a) option) -> 'a -> t -> t *)
  let add_next nexter elt t = begin 
    let new_trie = Trie.add_next nexter elt t.trie in
    let new_list = begin 
      match nexter elt with
        None -> t.list
      | Some (pre, elt') ->
          add_next_aux nexter (
            LitlDeque.cons pre LitlDeque.empty
          ) elt' [] t.list
    end
    in {
      trie = new_trie ;
      list = new_list
    }
  end
  
  (* type 'a elt = Nil | Cons of 'a * 'a t
  and 'a t = 'a elt ref *)

  let remove keys t = begin 

    let rec aux keys trie tlist mlist = begin 
      match keys with
        [] -> begin 
          if trie.Trie.present then begin 
            let trie' = { trie with Trie.present = false } in
            (trie', tlist)
          end
          else raise Eject (* nothing to do *)
        end
      | key :: keys' -> begin 
          match M.find_opt key trie.Trie.next with
            Some subtrie -> begin 
              match tlist with
                [] -> assert false
              | tt :: tlist' -> begin 

                (* We update our mutable list *)
                let mnext = ref MutableList.Nil in
                mlist := MutableList.Cons (key, mnext) ;

                (* We call aux on the next level *)
                let (subtrie', tlist'') = aux keys' subtrie tlist' mnext in

                (* We update our current trie *)
                let trie' = {
                  trie with Trie.next = begin 
                    if Trie.is_empty subtrie'
                    then (* begin Format.printf "..a\n" ; *) 
                      M.remove key trie.Trie.next (* end *)
                    else M.set key subtrie' trie.Trie.next
                  end } in
    
                (* We un-update (downdate?) our mutable list *)
                mlist := MutableList.Nil ;

                (* We now modify our trielist accordingly *)
                let tlist3 = begin 
                  if Trie.is_empty subtrie'
                  then begin 
                    let mlist = MutableList.cons key root in
                    let tt' =
                      Trie.remove_enum (MutableList.enum mlist) tt in
                    if Trie.is_empty tt' && tlist'' = []
                    then [] else tt' :: tlist''
                  end
                  else tt :: tlist''
                end
    
                (* We are done, now *)
                in (
                  trie',
                  tlist3
                )
              end
            end
          | None -> raise Eject (* nothing to do *)
      end
    end in
    let aux_first keys trie tlist mlist = begin 
      match keys with
        [] -> begin 
          if trie.Trie.present then begin 
            let trie' = { trie with Trie.present = false } in
            (trie', tlist)
          end
          else raise Eject (* nothing to do *)
        end
      | key :: keys' -> begin 
        match M.find_opt key trie.Trie.next with
          Some trie' -> begin 
            let mnext = ref MutableList.Nil in
            mlist := MutableList.Cons(key, mnext) ;
            let (trie'', tlist') = aux keys' trie' tlist mnext in (
              { trie with Trie.next = M.set key trie'' trie.Trie.next },
              tlist'
            )
          end
        | None -> raise Eject (* nothing to do *)
      end
    end in
    try
      let (trie, tlist) = aux_first keys t.trie t.list root in
      { trie = trie ; list = tlist }
    with Eject -> t
  end

  let remove_enum keys t = begin 

    let root = ref MutableList.Nil in

    let rec aux keys trie tlist mlist = begin 
      match Enum.next keys with
        None -> begin 
          if trie.Trie.present then begin 
            let trie' = { trie with Trie.present = false } in
            (trie', tlist)
          end
          else raise Eject (* nothing to do *)
        end
      | Some (key, keys') -> begin 
          match M.find_opt key trie.Trie.next with
            Some subtrie -> begin 
              match tlist with
                [] -> assert false
              | tt :: tlist' -> begin 

                (* We update our mutable list *)
                let mnext = ref MutableList.Nil in
                mlist := MutableList.Cons (key, mnext) ;

                (* We call aux on the next level *)
                let (subtrie', tlist'') = aux keys' subtrie tlist' mnext in

                (* We update our current trie *)
                let trie' = {
                  trie with Trie.next = begin 
                    if Trie.is_empty subtrie'
                    then
                      M.remove key trie.Trie.next
                    else M.set key subtrie' trie.Trie.next
                  end
               } in
    
                (* We un-update (downdate?) our mutable list *)
                mlist := MutableList.Nil ;

                (* We now modify our trielist accordingly *)
                let tlist3 = begin 
                  if Trie.is_empty subtrie'
                  then begin 
                    let mlist = MutableList.cons key root in
                    let tt' = Trie.remove_enum (MutableList.enum mlist) tt in
                    if Trie.is_empty tt' && tlist'' = []
                    then [] else tt' :: tlist''
                  end
                  else tt :: tlist''
                end
    
                (* We are done, now *)
                in (
                  trie',
                  tlist3
                )
              end
            end
          | None -> raise Eject (* nothing to do *)
      end
    end in
    let aux_first keys trie tlist mlist = begin 
      match Enum.next keys with
        None -> begin 
          if trie.Trie.present then begin 
            let trie' = { trie with Trie.present = false } in
            (trie', tlist)
          end
          else raise Eject (* nothing to do *)
        end
      | Some (key, keys') -> begin 
        match M.find_opt key trie.Trie.next with
          Some trie' -> begin 
            let mnext = ref MutableList.Nil in
            mlist := MutableList.Cons(key, mnext) ;
            let (trie'', tlist') = aux keys' trie' tlist mnext in (
              { trie with Trie.next = M.set key trie'' trie.Trie.next },
              tlist'
            )
          end
        | None -> raise Eject (* nothing to do *)
      end
    end in
    try
      let (trie, tlist) = aux_first keys t.trie t.list root in
      { trie = trie ; list = tlist }
    with Eject -> t
  end
  
  let change f keys t = begin 
    match f (Trie.mem keys t.trie) with
      false -> remove keys t
    | true -> add keys t
  end

  let change_enum f keys t = begin 
    let keys' = Enum.memo keys in
    match f (Trie.mem_enum keys' t.trie) with
      false -> remove_enum keys' t
    | true -> add_enum keys' t
  end

  let change f keys t = begin 

(* Dans le cas true, deux cas : on est au bout de keys, et il ne faut que
   revenir, ou bien d'une part il faut venir les keys puis revenir.
   Il faut donc avoir deux fonctions "positives" :
   - on continue (éventuellement) de parcourir les clés, puis...
   - on dépile.
 *)

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
	  atom list -> atom deque -> trie list ->
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

  (* When false, we just need to go down. *)

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

  let change f keys t = begin 
    change_aux f keys t.trie LitlDeque.empty t.list []
  end


  let change_return f keys t = begin 
    let (flag, result) = f (Trie.mem keys t.trie) in
    match flag with
      false -> (remove keys t, result)
    | true -> (add keys t, result)
  end

  let change_return_enum f keys t = begin 
    let (flag, result) = f (Trie.mem_enum keys t.trie) in
    match flag with
      false -> (remove_enum keys t, result)
    | true -> (add_enum keys t, result)
  end

  let singleton elt = begin 
    add elt empty
  end
  
  (* let rec union t1 t2 = {
    present = t1.present || t2.present ;
    next = M.fold (
      fun index t next -> M.change (
        fun t_opt -> match t_opt with
          None -> Some t
        | Some t' -> Some (union t t')
        ) index next
      ) t1.next t2.next
  } *)

  (* let rec inter t1 t2 = {
    present = t1.present && t2.present ;
    next = Enum.fold (
      fun key next -> 
        match M.find_opt key t2.next with 
          None -> M.remove key next
        | Some t' -> M.change (
            function
              None -> None
            | Some t -> let t'' = inter t t' in
                if is_empty t'' then None else Some t''
          ) key next
    ) (M.keys t1.next) t1.next
  } *)

  (* let rec diff t1 t2 = {
    present = t1.present && not t2.present ;
    next = Enum.fold (
      fun key next -> 
        match M.find_opt key t2.next with 
          None -> next
        | Some t' -> M.change (
            function
              None -> None
            | Some t -> let t'' = diff t t' in
                if is_empty t'' then None else Some t''
          ) key next
    ) (M.keys t1.next) t1.next
  } *)

  let subset t1 t2 = begin 
    Trie.subset t1.trie t2.trie
  end
    
  let enum t = begin 
    Trie.enum t.trie
  end

  let iter f t = begin 
    Trie.iter f t.trie
  end

  let fold f t accu = begin 
    Trie.fold f t.trie accu
  end

  let for_all f t = begin 
    Trie.for_all f t.trie
  end  

  let exists f t = begin 
    Trie.exists f t.trie
  end

  let compare t1 t2 = begin 
    Trie.compare t1.trie t2.trie
  end

  let equal t1 t2 = begin 
    Trie.equal t1.trie t2.trie
  end

  let choose t = begin 
    Trie.choose t.trie
  end

  let min t = begin 
    Trie.min t.trie
  end

  let max t = begin 
    Trie.max t.trie
  end
  
  (* let rec split keys t = begin 
    match keys with
      [] -> (empty, t.present, {present = false ; next = t.next})
    | key :: keys' -> begin 
        let (m1, t_opt, m2) = M.split key t.next in
          match t_opt with
            None -> ({
                present = t.present ;
                next = m1
              },
              false, {
                present = false ;
                next = m2
              }
            )
          | Some t' -> 
            let (t1, pres, t2) = split keys' t' in
            ({
              present = t.present ;
              next = if (is_empty t1) then m1 else M.set key t1 m1
            },
            pres, {
              present = false ;
              next = if (is_empty t2) then m2 else M.set key t2 m2
            }
          )
    end
  end *)

  (* let rec split_left keys t = begin 
    match keys with
      [] -> (empty, t.present)
    | key :: keys' -> begin 
        let (m1, t_opt) = M.split_left key t.next in
          match t_opt with
            None ->
              (
                {
                  present = t.present ;
                  next = m1
                },
                false
              )
          | Some t' -> 
            let (t1, pres) = split_left keys' t' in
            (
              {
                present = t.present ;
                next = if (is_empty t1) then m1 else M.set key t1 m1
              },
              pres
            )
    end
  end *)

  (* let rec split_right keys t = begin 
    match keys with
      [] -> (t.present, {present = false ; next = t.next})
    | key :: keys' -> begin 
        let (t_opt, m2) = M.split_right key t.next in
          match t_opt with
            None -> (
              false, {
                present = false ;
                next = m2
              }
            )
          | Some t' -> 
            let (pres, t2) = split_right keys' t' in (
              pres, {
                present = false ;
                next = if (is_empty t2) then m2 else M.set key t2 m2
              }
            )
    end
  end *)

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

List.map f tt.list ;;

LitlEnum.to_list (Trie.enum tt.trie) ;;

let tt = remove [1;3] tt ;;
let tt = remove [1;2] tt ;;
let tt = remove [1;2;3] tt ;;
let tt = remove [1;3;4] tt ;;

*)