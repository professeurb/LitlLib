(************************************************************************
*
*  Trie.ml
*  
*  Basically, it is a unit TrieMap.t
*
*  Created by Olivier Brunet on 15 Jul 2010.
*  Copyright (c) 2010 B.W.C. Computing. All rights reserved.
*
************************************************************************)

type 'a enum = 'a LitlEnumerator.enum

module type SET = LitlPervasives.SET

module type MAP = LitlPervasives.MAP

module type TRIE = sig 
  type pre_elt
  include SET

  val add_enum : pre_elt enum -> t -> t
end

module Make (M : MAP) = struct 
  type pre_elt = M.index
	type elt = M.index list
	type t = { 
		present : bool ;
		next : t M.t
	}
	
	let empty = { 
	  present = false ;
	  next = M.empty
  }

	let is_empty t = begin 
		(not t.present) && M.is_empty t.next
	end

  (* Needs more thinking about it *)
  let rec height t = begin 
    if t.present
    then begin 
      let h = M.height t.next in
      if h > 1
      then h + 1
      else 1 + (let (_, t') = M.choose t.next in height t') 
	  end
	  else begin 
		  let h = M.height t.next in
      if h > 1
      then h
      else let (_, t') = M.choose t.next in height t'
    end
  end

  let zoom t = begin 
    let rec zoom_aux t elts = begin 
	    if t.present
	    then begin 
	      if M.is_empty t.next
	      then LitlZoom.One (List.rev elts)
        else LitlZoom.More
      end
      else begin 
        match M.zoom t.next with
          LitlZoom.One (elt, t') -> zoom_aux t' (elt :: elts)
        | LitlZoom.Zero -> LitlZoom.Zero
				| LitlZoom.More -> LitlZoom.More
      end
    end in
    zoom_aux t []
  end
	
	let rec mem elt t = begin 
		match elt with
			[] -> t.present
		| index :: elt' ->
				match M.find_opt index t.next with
					None -> false
				| Some t' -> mem elt' t'
	end
	
	let rec add elt t = begin 
		match elt with
			[] -> { present = true ; next = t.next }
		| index :: elt' ->
			let next' = M.change (
			fun t_opt -> Some (
				add elt' (
					match t_opt with
						None -> empty
					| Some t -> t
				)
			)
		) index t.next in {
			present = t.present ;
			next = next'
		}
	end

	let rec add_enum elt t = begin 
		match LitlEnumerator.next elt with
			None -> { present = true ; next = t.next }
		| Some (index, elt') ->
			let next' = M.change (
			fun t_opt -> Some (
				add_enum elt' (
					match t_opt with
						None -> empty
					| Some t -> t
				)
			)
		) index t.next in {
			present = t.present ;
			next = next'
		}
	end

	let rec remove keys t = begin 
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
	end

  let rec change f keys t = begin 
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
  end

  let rec change_return f keys t = begin 
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
  end

	let singleton elt = begin 
		add elt empty
	end
	
	let rec union t1 t2 = {
		present = t1.present || t2.present ;
		next = M.fold (
			fun index t next -> M.change (
				fun t_opt -> match t_opt with
					None -> Some t
				| Some t' -> Some (union t t')
				) index next
			) t1.next t2.next
	}

	let rec inter t1 t2 = {
		present = t1.present && t2.present ;
		next = LitlEnumerator.fold (
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
	}

	let rec diff t1 t2 = {
		present = t1.present && not t2.present ;
		next = LitlEnumerator.fold (
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
	}

	let subset t1 t2 = begin
		is_empty (diff t1 t2)
	end
		
	let enum t = begin 
		let rec aux accu t = begin 
			match t.present with
				false -> begin 
					LitlEnumerator.expand (
						fun (key, t') -> aux (key :: accu) t'
					) (M.enum t.next)
				end
			| true -> begin 
						LitlEnumerator.from_once (
							fun () -> Some (
								List.rev accu,
								LitlEnumerator.expand (
									fun (key, t') -> aux (key :: accu) t'
								) (M.enum t.next)
							)
						)
			end
		end in
		aux [] t
	end

	let iter f t = begin 
		let rec aux keys t = begin 
			begin 
			  match t.present with
				  false -> ()
			  | true -> f (List.rev keys)
			end ;
			M.iter (fun key t' -> aux (key :: keys) t') t.next
		end in
		aux [] t
	end

	let fold f t accu = begin 
		let rec aux keys t accu = begin 
			let accu' = begin 
				match t.present with
					false -> accu
				| true -> f (List.rev keys) accu
			end in
			M.fold (fun key t' a -> aux (key :: keys) t' a) t.next accu'
		end in
		aux [] t accu
	end
	
	let for_all f t = begin 
		let rec aux keys t = begin 
			match t.present with
				false -> M.for_all (fun key t' -> aux (key :: keys) t') t.next
			| true ->
					(f (List.rev keys)) 
					&& (M.for_all (fun key t' -> aux (key :: keys) t') t.next)
		end in
		aux [] t
	end

	let exists f t = begin 
		let rec aux keys t = begin 
			match t.present with
				false -> M.exists (fun key t' -> aux (key :: keys) t') t.next
			| true ->
					(f (List.rev keys)) 
					|| (M.exists (fun key t' -> aux (key :: keys) t') t.next)
		end in
		aux [] t
	end

	let compare t1 t2 = begin 
		let rec aux t1 t2 = begin 
			match (t1.present, t2.present) with
				false, true -> 1
			| true, false -> -1
			| _, _ -> M.compare aux t1.next t2.next
		end in
		aux t1 t2
	end

	let equal t1 t2 = begin 
		let rec aux t1 t2 = begin 
			match (t1.present, t2.present) with
				false, true _ -> false
			| true, false -> false
			| _, _ -> M.equal aux t1.next t2.next
		end in
		aux t1 t2
	end

	let choose t = begin 
		let rec aux keys t = begin 
			match t.present with
				false -> let (key, t') = M.choose t.next in aux (key :: keys) t'
			| true -> List.rev keys
		end in
		aux [] t
	end

	let min t = begin 
		let rec aux keys t = begin 
			match t.present with
				false -> let (key, t') = M.min t.next in aux (key :: keys) t'
			| true -> List.rev keys
		end in
		aux [] t
	end

	let max t = begin 
		let rec aux keys t = begin 
			match t.present with
				false -> let (key, t') = M.max t.next in aux (key :: keys) t'
			| true -> List.rev keys
		end in
		aux [] t
	end
	
	let rec split keys t = begin 
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
	end

	let rec split_left keys t = begin 
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
	end

	let rec split_right keys t = begin 
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
	end

	let filter f t = begin 
		fold (fun elt t -> if f elt then add elt t else t) t empty
	end
end

module BiList = struct 
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
	  LitlEnumerator.from_generator aux t
	end
end

(* 
module M = Litl.Map (
  struct
    type t = int
    let compare = compare
    let equal = (=)
  end)
*)

module Make_Queriable (M : MAP) = struct
  module Trie = Make (M)

  type t = {
    trie : Trie.t ;
    list : Trie.t list
  }

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
          Trie.add_enum (BiList.enum (BiList.cons pre head)) trie
        end in
        let new_tail = ref BiList.Nil in
        tail := BiList.Cons (pre, new_tail) ;
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
		      let tail = ref BiList.Nil in
		      add_aux (BiList.cons pre tail) tail elt' [] t.list
	    end
		end
		in {
      trie = new_trie ;
		  list = new_list
    }
	end

  let rec add_enum_aux head tail enum do_ne to_do = begin 
	  match Enumerator.next enum with
	    Some (pre, enum') -> begin 
		    let (trie, to_do') = match to_do with
		      [] -> (Trie.empty, [])
		    | trie :: to_do' -> (trie, to_do') in 
        let trie' = begin 
          Trie.add_enum (BiList.enum (BiList.cons pre head)) trie
        end in
        let new_tail = ref BiList.Nil in
        tail := BiList.Cons (pre, new_tail) ;
        add_enum_aux head new_tail enum' (trie' :: do_ne) to_do'
      end
	  | _ -> List.rev_append do_ne to_do
	end

  let add_enum elt t = begin 
    let enum = Enumerator.memo elt in
    let new_trie = Trie.add_enum enum t.trie in
    let new_list = begin 
	    match Enumerator.next enum with
	      None -> t.list
	    | Some (pre, enum') -> begin 
	        let tail = ref BiList.Nil in
		      add_enum_aux (BiList.cons pre tail) tail enum' [] t.list
	    end
	  end
    in {
      trie = new_trie ;
		  list = new_list
    }
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
		next = LitlEnumerator.fold (
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
		next = LitlEnumerator.fold (
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

let t1 = empty ;;
let t1 = add [1;2] t1 ;;
let t1 = add [2;3;4] t1 ;;
let t1 = add [1;2;3] t1 ;;

let t2 = empty ;;
let t2 = add [1;2;4] t2 ;;
let t2 = add [1;3;4] t2 ;;
let t2 = add [2;3;4] t2 ;;

LitlEnumerator.to_list (enum t1) ;;
LitlEnumerator.to_list (enum t2) ;;
LitlEnumerator.to_list (enum (inter t1 t2)) ;;
LitlEnumerator.to_list (enum (union t1 t2)) ;;

LitlEnumerator.to_list (enum (diff t1 t2)) ;;
LitlEnumerator.to_list (enum (diff t2 t1)) ;;

*)