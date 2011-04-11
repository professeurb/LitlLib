(************************************************************************
*
*  litlQueryTrie.ml
*  
*
*  Created by Olivier Brunet on 29 Mar 2011.
*  Copyright (c) 2011 B.W.C. Computing. All rights reserved.
*
************************************************************************)

(* module Enum = Litl.Core.Enumerator *)

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
	  Enum.from_generator aux t
	end
end

(*

module I = struct type t = int let compare = compare let equal = (=) end
module M = Litl.Map (I) ;;
*)

module Make (M : MAP) = struct
  module Trie = LitlTrie.Make (M)

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

  let rec add_aux head tail elt do_ne to_do = begin 
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
	end

  let add elt t = begin 
	  let new_trie = Trie.add elt t.trie in
	  let new_list = begin 
		  match elt with
		    [] -> t.list
		  | pre :: elt' -> begin 
		      let tail = ref MutableList.Nil in
		      add_aux (MutableList.cons pre tail) tail elt' [] t.list
	    end
		end
		in {
      trie = new_trie ;
		  list = new_list
    }
	end

  let rec add_enum_aux head tail enum do_ne to_do = begin 
	  match Enum.next enum with
	    Some (pre, enum') -> begin 
		    let (trie, to_do') = match to_do with
		      [] -> (Trie.empty, [])
		    | trie :: to_do' -> (trie, to_do') in 
        let trie' = begin 
          Trie.add_enum (MutableList.enum (MutableList.cons pre head)) trie
        end in
        let new_tail = ref MutableList.Nil in
        tail := MutableList.Cons (pre, new_tail) ;
        add_enum_aux head new_tail enum' (trie' :: do_ne) to_do'
      end
	  | _ -> List.rev_append do_ne to_do
	end

  let add_enum elt t = begin 
    let enum = Enum.memo elt in
    let new_trie = Trie.add_enum enum t.trie in
    let new_list = begin 
	    match Enum.next enum with
	      None -> t.list
	    | Some (pre, enum') -> begin 
	        let tail = ref MutableList.Nil in
		      add_enum_aux (MutableList.cons pre tail) tail enum' [] t.list
	    end
	  end
    in {
      trie = new_trie ;
		  list = new_list
    }
	end
	
	(* type 'a elt = Nil | Cons of 'a * 'a t
  and 'a t = 'a elt ref *)

  let rec remove keys t = begin 
    let root = ref MutableList.Nil in

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
		            (* let tt' = begin 
		              if Trie.is_empty subtrie'
		              then begin 
			              let mlist = MutableList.cons key root in
			              let mlenum = MutableList.enum mlist in
		                (* Enum.iter (fun i -> Format.printf "%d " i) mlenum ;
		                Format.printf "...\n" ; *)
			              Trie.remove (List.rev (Enum.to_list mlenum)) tt
			            end
		              else tt
		            end *)
		            let tlist3 = begin 
		              if Trie.is_empty subtrie'
		              then begin 
			              let mlist = MutableList.cons key root in
			              let mlenum = MutableList.enum mlist in
		                (* Enum.iter (fun i -> Format.printf "%d " i) mlenum ;
		                Format.printf "...\n" ; *)
			              let tt' =
			                Trie.remove (List.rev (Enum.to_list mlenum)) tt in
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
	
	(* let rec remove keys t = begin 
		match keys with
			[] -> { present = false ; next = t.next }
		| key :: keys' -> {
				present = t.present ;
				next = M.change (function
					None -> None
				| Some t' -> 
						let t'' = remove keys' t' in
						if is_empty t'' then None else Some t''
				) key t.next
			}
	end *)

  (* let rec change f keys t = begin 
    match keys with
      [] -> { t with present = f t.present }
    | key :: keys' -> begin 
        let next' = M.change (
          function
            None -> if f false then Some (add keys' empty) else None
          | Some t' ->
	            let t'' = change f keys' t' in 
	            if is_empty t'' then None else Some t''
        ) key t.next in
        { t with next = next' }
    end
  end *)

  (* let rec change_return f keys t = begin 
    match keys with
      [] -> begin 
        let (flag, result) = f t.present in
        ({ t with present = flag }, result)
      end
    | key :: keys' -> begin 
        let (next', result) = M.change_return (
          function
            None -> begin 
              let (flag, result) = f false in
              if flag
              then (Some (add keys' empty), result)
              else (None, result)
            end
          | Some t' ->
	            let (t'', result) = change_return f keys' t' in 
	            if is_empty t'' then (None, result) else (Some t'', result)
        ) key t.next in
        ({ t with next = next' }, result)
    end
  end *)

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

let f t = Enum.to_list (Trie.enum t) ;;

let tt = empty ;;
let tt = add [1;2;3] tt ;;
let tt = add [1;3;4] tt ;;
let tt = add [1;3] tt ;;

List.map f tt.list ;;

let tt = remove [1;3] tt ;;
let tt = remove [1;2] tt ;;
let tt = remove [1;2;3] tt ;;
let tt = remove [1;3;4] tt ;;

*)