(************************************************************************
*
*  TrieMap.ml
*  
*
*  Created by Olivier Brunet on 9 Jun 2010.
*  Copyright (c) 2010 B.W.C. Computing. All rights reserved.
*
************************************************************************)

type 'a enum = 'a LitlEnumerator.enum

module type MAP = LitlPervasives.MAP

module type TRIE_MAP = sig 
	include MAP

  type index_elt

	val extract : index -> 'a t -> 'a enum
	val extract_enum : index_elt enum -> 'a t -> 'a enum
end

module Make (M : MAP) : TRIE_MAP with
  type index_elt = M.index and
	type index = M.index list
= struct 
  type index_elt = M.index
	type index = M.index list
	type 'a elt = index * 'a
	type 'a t = { 
		value : 'a option ;
		next : 'a t M.t
	}
	
	let empty = {
		value = None ;
		next = M.empty
	}

	let is_empty t = begin 
		(t.value = None) && (M.is_empty t.next)
	end
	
	let rec height t = begin 
    match t.value with
      Some _ -> begin 
      let h = M.height t.next in
      if h > 1
      then h + 1
      else 1 + (let (_, t') = M.choose t.next in height t') 
	  end
	  | _ -> begin 
		  let h = M.height t.next in
      if h > 1
      then h
      else let (_, t') = M.choose t.next in height t'
    end
  end
  
  let zoom t = begin 
    let rec zoom_aux t elts = begin 
	    match t.value with
	    | Some value -> begin 
	      if M.is_empty t.next
	      then LitlZoom.One (List.rev elts, value)
        else LitlZoom.More
      end
      | None -> begin 
        match M.zoom t.next with
          LitlZoom.One (elt, t') -> zoom_aux t' (elt :: elts)
        | LitlZoom.Zero -> LitlZoom.Zero
				| LitlZoom.More -> LitlZoom.More
      end
    end in
    zoom_aux t []
  end
	
	let rec set keys value t = begin 
		match keys with
		[] -> { value = Some value ; next = t.next }
	| key :: keys' -> {
			value = t.value ;
			next = M.change (
				fun t_opt -> 
					Some (
						set keys' value (
						match t_opt with 
							None -> empty 
						| Some t' -> t'
						)
					)
				) key t.next
			}
	end

	let rec find keys t = begin 
		match keys with
		[] -> begin 
			match t.value with
				None -> raise Not_found
			| Some value -> value
		end
	| key :: keys' -> find keys' (M.find key t.next)
	end
	
	let rec find_opt keys t = begin 
		match keys with
			[] -> t.value
		| key :: keys' -> begin 
				match M.find_opt key t.next with
					None -> None
				| Some t' -> find_opt keys' t'
		end
	end

	let rec remove keys t = begin 
		match keys with
			[] -> { value = None ; next = t.next }
		| key :: keys' -> {
				value = t.value ;
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
			[] -> { value = f t.value ; next = t.next }
		| key :: keys' -> {
				value = t.value ;
				next = M.change (function
						None -> begin 
							match f None with
								None -> None
							| Some value -> Some (set keys' value empty)
						end
					| Some t' -> 
							let t'' = change f keys' t' in
							if is_empty t'' then None else Some t''
				) key t.next
			}
	end

	let change_return f keys t = begin 
		let res_opt = ref None in
		let rec change_aux keys t = begin 
			match keys with
				[] -> let (value', res) = f t.value in begin 
						res_opt := Some res ;
						{ value = value' ; next = t.next }
				end
			| key :: keys' -> {
					value = t.value ;
					next = M.change (function
							None -> begin 
								match f None with
									(None, res) -> begin
										res_opt := Some res ;
										None
									end
								| (Some value, res) -> begin
										res_opt := Some res ;
										Some (set keys' value empty)
								end
							end
						| Some t' -> 
								let t'' = change_aux keys' t' in
								if is_empty t'' then None else Some t''
					) key t.next
				}
		end in
		let t' = change_aux keys t in
		match !res_opt with
			None -> assert false
		| Some res -> (t', res)
	end

	let rec has_key keys t = begin 
		match keys with
			[] -> begin 
				match t.value with
					None -> false
				| Some v -> true
			end
		| key :: keys' -> begin 
				match M.find_opt key t.next with
					None -> false
				| Some t' -> has_key keys' t'
		end
	end

	let enum t = begin 
		let rec aux accu t = begin 
			match t.value with
				None -> begin 
					LitlEnumerator.expand (fun (key, t') -> 
						aux (key :: accu) t'
					) (M.enum t.next)
				end
			| Some v -> begin 
						LitlEnumerator.from_once (fun () -> 
							Some (
								(List.rev accu, v),
								LitlEnumerator.expand (fun (key, t') -> 
									aux (key :: accu) t'
								) (M.enum t.next)
							)
						)
			end
		end in
		aux [] t
	end

	let iter f t = begin 
		let rec aux keys t = begin 
			match t.value with
				None -> ()
			| Some value -> f (List.rev keys) value ;
			M.iter (fun key t' -> aux (key :: keys) t') t.next
		end in
		aux [] t
	end

	let fold f t accu = begin 
		let rec aux keys t accu = begin 
			let accu' = begin 
				match t.value with
					None -> accu
				| Some value -> f (List.rev keys) value accu
			end in
			M.fold (fun key t' a -> aux (key :: keys) t' a) t.next accu'
		end in
		aux [] t accu
	end
	
	let for_all f t = begin 
		let rec aux keys t = begin 
			match t.value with
				None -> M.for_all (fun key t' -> aux (key::keys) t') t.next
			| Some value ->
					(f (List.rev keys) value) 
					&& (M.for_all (fun key t' -> aux (key::keys) t') t.next)
		end in
		aux [] t
	end

	let exists f t = begin 
		let rec aux keys t = begin 
			match t.value with
				None -> M.exists (fun key t' -> aux (key::keys) t') t.next
			| Some value ->
					(f (List.rev keys) value) 
					|| (M.exists (fun key t' -> aux (key::keys) t') t.next)
		end in
		aux [] t
	end

	let keys t = begin 
		let rec aux accu t = begin 
			match t.value with
				None -> begin 
					LitlEnumerator.expand (fun (key, t') -> 
						aux (key :: accu) t'
					) (M.enum t.next)
				end
			| Some _ -> begin 
						LitlEnumerator.from_once (fun () -> 
							Some (
								(List.rev accu),
								LitlEnumerator.expand (fun (key, t') -> 
									aux (key :: accu) t'
								) (M.enum t.next)
							)
						)
			end
		end in
		aux [] t
	end
	
	let values t = begin 
		let rec aux t = begin 
			match t.value with
				None -> begin 
					LitlEnumerator.expand (fun (_, t') -> 
						aux t'
					) (M.enum t.next)
				end
			| Some v -> begin 
						LitlEnumerator.from_once (fun () -> 
							Some (
								v,
								LitlEnumerator.expand (fun (_, t') -> 
									aux t'
								) (M.enum t.next)
							)
						)
			end
		end in
		aux t
	end
	
	let rec map f t = begin { 
			value = begin 
				match t.value with
					None -> None
				| Some v -> Some (f v)
			end ;
			next = M.map (fun t -> map f t) t.next
		}
	end

	let rec map_opt f t = begin { 
			value = begin 
				match t.value with 
					None -> None
				| Some v -> f v
			end ;
			next = M.map_opt (fun t -> begin 
				let t' = map_opt f t in
				if is_empty t' then None else Some t'
			end
			) t.next
		}
	end

	let map_with_key f t = begin 
		let rec aux keys t = begin { 
				value = begin 
					match t.value with
						None -> None
					| Some v -> Some (f (List.rev keys) v)
				end ;
				next = M.map_with_key (fun key t -> aux (key :: keys) t) t.next
			}
		end in
		aux [] t
	end

	let map_opt_with_key f t = begin 
		let rec aux keys t = begin { 
				value = begin 
					match t.value with 
						None -> None
					| Some v -> f (List.rev keys) v
				end ;
				next = M.map_opt_with_key (fun key t -> begin 
					let t' = aux (key :: keys) t in
					if is_empty t' then None else Some t'
				end
				) t.next
			}
		end in
		aux [] t
	end

	let compare comp t1 t2 = begin 
		let rec aux t1 t2 = begin 
			match (t1.value, t2.value) with
				None, Some _ -> 1
			| Some _, None -> -1
			| None, None -> M.compare aux t1.next t2.next
			| Some v1, Some v2 -> begin 
				  let c = comp v1 v2 in
					if c = 0 then M.compare aux t1.next t2.next else c
			end
		end in
		aux t1 t2
	end

	let equal equ t1 t2 = begin 
		let rec aux t1 t2 = begin 
			match (t1.value, t2.value) with
				None, Some _ -> false
			| Some _, None -> false
			| None, None -> M.equal aux t1.next t2.next
			| Some v1, Some v2 -> (equ v1 v2) && (M.equal aux t1.next t2.next)
		end in
		aux t1 t2
	end

	let choose t = begin 
		let rec aux keys t = begin 
			match t.value with
				None -> let (key, t') = M.choose t.next in aux (key :: keys) t'
			| Some value -> (List.rev keys, value)
		end in
		aux [] t
	end

	let min t = begin 
		let rec aux keys t = begin 
			match t.value with
				None -> let (key, t') = M.min t.next in aux (key :: keys) t'
			| Some value -> (List.rev keys, value)
		end in
		aux [] t
	end

	let max t = begin 
		let rec aux keys t = begin 
			match t.value with
				None -> let (key, t') = M.max t.next in aux (key :: keys) t'
			| Some value -> (List.rev keys, value)
		end in
		aux [] t
	end
	
	let rec split keys t = begin 
		match keys with
			[] -> (empty, t.value, {value = None ; next = t.next})
		| key :: keys' -> begin 
				let (m1, t_opt, m2) = M.split key t.next in
					match t_opt with
						None -> ({
								value = t.value ;
								next = m1
							},
							None, {
								value = None ;
								next = m2
							}
						)
					| Some t' -> 
						let (t1, v_opt, t2) = split keys' t' in
						({
							value = t.value ;
							next = if (is_empty t1) then m1 else M.set key t1 m1
						},
						v_opt, {
							value = None ;
							next = if (is_empty t2) then m2 else M.set key t2 m2
						}
					)
		end
	end

	let rec split_right keys t = begin 
		match keys with
			[] -> (t.value, {value = None ; next = t.next})
		| key :: keys' -> begin 
				let (t_opt, m2) = M.split_right key t.next in
					match t_opt with
						None -> (
							None, {
								value = None ;
								next = m2
							}
						)
					| Some t' -> let (v_opt, t2) = split_right keys' t' in (
						v_opt, {
							value = None ;
							next = if (is_empty t2) then m2 else M.set key t2 m2
						}
					)
		end
	end

	let rec extract_aux l map e = begin 
		match l with
			[] -> e
		| a :: l' -> begin 
				match (M.split_right a map) with
					(None, m') -> extract_aux l' m' e
				|	(Some entry, m') -> begin 
					 match entry.value with
						None -> extract_aux l' entry.next (extract_aux l' m' e)
					| Some v -> LitlEnumerator.from_once (
							fun () -> Some (
								v, 
								extract_aux l' entry.next (extract_aux l' m' e)
							)
						)
				end
 		end
	end
	(* val extract : index -> 'a t -> 'a enum *)
	let extract l t = begin 
		match t.value with
				None -> extract_aux l t.next (LitlEnumerator.empty)
			| Some v -> extract_aux l t.next (LitlEnumerator.from_list [v])
	end

	let rec extract_aux_enum l map e = begin 
		match LitlEnumerator.next l with
			None -> e
		| Some (a, l') -> begin 
				match (M.split_right a map) with
					(None, m') -> extract_aux_enum l' m' e
				|	(Some entry, m') -> begin 
					 match entry.value with
						None -> extract_aux_enum l' entry.next (extract_aux_enum l' m' e)
					| Some v -> LitlEnumerator.from_once (
							fun () -> Some (
								v, 
								extract_aux_enum l' entry.next (extract_aux_enum l' m' e)
							)
						)
				end
 		end
	end
	(* val extract_enum : index_elt enum -> 'a t -> 'a enum *)
	let extract_enum e t = begin 
		match t.value with
				None -> extract_aux_enum (
					LitlEnumerator.memo e
			  ) t.next (LitlEnumerator.empty)
			| Some v -> extract_aux_enum (
				  LitlEnumerator.memo e
				) t.next (LitlEnumerator.from_list [v])
	end

end

(*

let t = empty ;;
let t = set [1; 2] 1 t ;;
let t = set [1; 3] 2 t ;;
let t = set [2] 3 t ;;
let t = set [4] 4 t ;;

LitlEnumerator.to_list (extract [2; 3; 4] t) ;;

let int_to_list n =
	let rec aux n acc =
		if n = 0 then acc else aux (n/10) ((n mod 10)::acc)
	in aux n [] ;;

let rec repeat f n o = if n = 0 then o else repeat f (n-1) (f o) ;;

let m = repeat (
	fun m -> 
		let n = Random.int 10000 in
		set (int_to_list n) n m
	) 500 empty ;;

let cons a b = a :: b ;;

LitlEnumerator.fold cons (values m) [] ;;

let (m1, r, m2) = split [2;2;1] m ;;

LitlEnumerator.fold cons (values m1) [] ;;

let oppen m = fold (fun k v l -> (k,v)::l) m [] ;;

*)