(************************************************************************
*
*  LitlMap.ml
*  
*
*  Created by Olivier Brunet on 10 May 2010.
*  Copyright (c) 2010 B.W.C. Computing. All rights reserved.
*
************************************************************************)

(*

#load "BinaryTree.cmo" ;;
#load "BinaryTreeMap.cmo" ;;
#load "LitlEnumerator.cmo" ;;

module I = struct
	type t = int
	let compare = compare
end

*)

type 'a enum = 'a LitlEnumerator.enum

module type OrderedType = LitlPervasives.ORDERED_TYPE

module type Map = LitlPervasives.MAP

module Make (I : OrderedType) : Map with
	type index = I.t
= struct
	module BTM = LitlBinaryTreeMap

	type index = I.t
	type 'a elt = index * 'a
	type 'a t = (index, 'a) BTM.t

	exception Eject	

	let height = function
		BTM.Empty -> 0
	| BTM.Node(_, _, _, _, h) -> h

	let create l k v r = begin 
		let hl = height l
		and hr = height r in
		BTM.Node (l, k, v, r, (if hl >= hr then hl + 1 else hr + 1))
	end
	
	let bal l k v r = begin 
		let hl = height l
		and hr = height r in
		if hl > hr + 2
		then begin 
			match l with
				BTM.Empty -> invalid_arg "Map.bal"
			| BTM.Node(ll, lv, ld, lr, _) ->
					if height ll >= height lr
					then create ll lv ld (create lr k v r)
					else begin 
						match lr with
							BTM.Empty -> invalid_arg "Map.bal"
						| BTM.Node(lrl, lrv, lrd, lrr, _) ->
								create (create ll lv ld lrl) lrv lrd (create lrr k v r)
					end
		end
		else if hr > hl + 2
		then begin 
			match r with
				BTM.Empty -> invalid_arg "Map.bal"
			| BTM.Node(rl, rv, rd, rr, _) ->
					if height rr >= height rl
					then create (create l k v rl) rv rd rr
					else begin 
						match rl with
							BTM.Empty -> invalid_arg "Map.bal"
						| BTM.Node(rll, rlv, rld, rlr, _) ->
								create (create l k v rll) rlv rld (create rlr rv rd rr)
					end
		end
		else BTM.Node(l, k, v, r, (if hl >= hr then hl + 1 else hr + 1))
	end
	
	let empty = BTM.Empty

	let is_empty = function
		BTM.Empty -> true
	| _ -> false

	let rec set key value = function
		BTM.Empty -> BTM.Node(BTM.Empty, key, value, BTM.Empty, 1)
	| BTM.Node(l, k, v, r, h) ->
			let c = I.compare key k in
			if c = 0
			then BTM.Node(l, k, value, r, h)
			else if c < 0
				then bal (set key value l) k v r
				else bal l k v (set key value r)

	let rec find key = function
		BTM.Empty -> raise Not_found
	| BTM.Node(l, k, v, r, _) ->
			let c = I.compare key k in
			if c = 0
			then v
			else find key (if c < 0 then l else r)

	let rec find_opt key = function
		BTM.Empty -> None
	| BTM.Node(l, k, v, r, _) ->
			let c = I.compare key k in
			if c = 0
			then Some v
			else find_opt key (if c < 0 then l else r)

	let rec has_key key = function
		BTM.Empty -> false
	| BTM.Node(l, k, _, r, _) ->
			let c = I.compare key k in
			c = 0 || has_key key (if c < 0 then l else r)

	let rec min_binding = function
		BTM.Empty -> raise Not_found
	| BTM.Node (BTM.Empty, k, v, _, _) -> (k, v)
	| BTM.Node (l, _, _, _, _) -> min_binding l

	let rec remove_min_binding = function
		BTM.Empty -> invalid_arg "Map.remove_min_elt"
	| BTM.Node (BTM.Empty, _, _, r, _) -> r
	| BTM.Node (l, k, v, r, _) -> bal (remove_min_binding l) k v r

	let min = min_binding

	let rec max = function
		BTM.Empty -> raise Not_found
	| BTM.Node (_, k, v, BTM.Empty, _) -> (k, v)
	| BTM.Node (_, _, _, r, _) -> max r

  let zoom = function
    BTM.Empty -> LitlZoom.Zero
  | BTM.Node (BTM.Empty, k, v, BTM.Empty, _) -> LitlZoom.One (k, v)
  | _ -> LitlZoom.More

	let merge t1 t2 = begin 
		match (t1, t2) with
			(BTM.Empty, t) -> t
		| (t, BTM.Empty) -> t
		| (_, _) ->
				let (k, v) = min_binding t2 in
				bal t1 k v (remove_min_binding t2)
	end
	
	(* val change : ('a option -> 'a option) -> index -> 'a t -> 'a t *)
	let change f key t = begin 
		let rec change_it = function
			BTM.Empty -> begin 
				match f None with
					None -> BTM.Empty
				| Some v -> BTM.Node(BTM.Empty, key, v, BTM.Empty, 1)
			end
		| BTM.Node (l, k, v, r, h) ->
			let c = I.compare key k in
			if c = 0 then begin 
				match f (Some v) with
					None -> merge l r
				| Some v' -> BTM.Node(l, k, v', r, h)
			end
			else if c < 0
				then bal (change_it l) k v r
				else bal l k v (change_it r)
		in change_it t
	end

	(* val change _return :
		('a option -> 'a option * 'b) -> index -> 'a t -> 'a t * 'b *)
	let change_return f key t = begin 
		let rec change_it = function
			BTM.Empty -> begin 
				match f None with
					None, res -> (BTM.Empty, res)
				| Some v, res -> (BTM.Node(BTM.Empty, key, v, BTM.Empty, 1), res)
			end
		| BTM.Node (l, k, v, r, h) ->
			let c = I.compare key k in
			if c = 0 then begin 
				match f (Some v) with
					None, res -> (merge l r, res)
				| Some v', res -> (BTM.Node(l, k, v', r, h), res)
			end
			else if c < 0
				then let (l', res) = change_it l in (bal l' k v r, res)
				else let (r', res) = change_it r in (bal l k v r', res)
		in change_it t
	end

	(* val remove : index -> 'a t -> 'a t *)
	let remove key t = begin 
		let rec remove_it = function
			BTM.Empty -> BTM.Empty
		| BTM.Node (l, k, v, r, h) ->
			let c = I.compare key k in
			if c = 0 then merge l r
			else if c < 0
				then bal (remove_it l) k v r
				else bal l k v (remove_it r)
		in 
		remove_it t
	end

	(* val fold : (index -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b *)
	let fold = BTM.fold

	(* val iter : (index -> 'a -> unit) -> 'a t -> unit *)
	let iter = BTM.iter

	(* val map : ('a -> 'b) -> 'a t -> 'b t *)
	let rec map f t = begin 
		match t with
			BTM.Empty -> BTM.Empty
		| BTM.Node (l, k, v, r, h) -> BTM.Node (map f l, k, f v, map f r, h)
	end

	(* val map_opt : ('a -> 'b option) -> 'a t -> 'b t *)
	let rec map_opt f t = begin 
		match t with
			BTM.Empty -> BTM.Empty
		| BTM.Node (l, k, v, r, h) -> begin 
			match f v with
				None -> merge (map_opt f l) (map_opt f r)
			| Some v' -> BTM.Node (map_opt f l, k, v', map_opt f r, h)
		end
	end

	(* val map_with_key : (index -> 'a -> 'b) -> 'a t -> 'b t *)
	let rec map_with_key f t = begin 
		match t with
			BTM.Empty -> BTM.Empty
		| BTM.Node(l, k, v, r, h) ->
				BTM.Node(map_with_key f l, k, f k v, map_with_key f r, h)
	end
	
	(* val map_opt_with_key : (index -> 'a -> 'b option) -> 'a t -> 'b t *)
	let rec map_opt_with_key f t = begin 
		match t with
			BTM.Empty -> BTM.Empty
		| BTM.Node (l, k, v, r, h) -> begin 
				match f k v with
					None -> merge (map_opt_with_key f l) (map_opt_with_key f r)
				| Some v' ->
						BTM.Node (map_opt_with_key f l, k, v', map_opt_with_key f r, h)
		end
	end

	(* val enum : 'a t -> (index * 'a) enum *)
	let enum m = LitlEnumerator.from_binary_tree_map m

	(* val keys : 'a t -> index enum *)
	let keys t = LitlEnumerator.from_binary_tree_map_keys t
	
	(* val values : 'a t -> 'a enum *)
	let values t = LitlEnumerator.from_binary_tree_map_values t

  let rec for_all p t = begin 
		match t with
      BTM.Empty -> true
    | BTM.Node(l, k, v, r, _) -> p k v && for_all p l && for_all p r
	end
	
  let rec exists p t = begin 
		match t with
      BTM.Empty -> false
    | BTM.Node(l, k, v, r, _) -> p k v || exists p l || exists p r
	end

	exception Diff of int

	(* val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int *)
	let compare comp s1 s2 = begin 
		try
			let e2 = fold (fun k1 v1 e2 -> begin 
				match LitlEnumerator.next e2 with
					None -> raise (Diff 1)
				| Some ((k2, v2), e2') -> begin 
						let c = I.compare k1 k2 in
						if c <> 0
						then raise (Diff c)
						else begin 
							let c2 = comp v1 v2 in
							if c2 <> 0
							then raise (Diff c2)
							else e2'
						end
				end
			end
			) s1 (enum s2) in
			match LitlEnumerator.next e2 with
				None -> 0
			| _ -> -1
		with Diff r -> r
	end

	(* val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool *)
	let equal comp s1 s2 = begin 
		try
			let e2 = fold (fun k1 v1 e2 -> begin 
				match LitlEnumerator.next e2 with
					None -> raise (Diff 1)
				| Some ((k2, v2), e2') -> begin 
						if (I.equal k1 k2) && (comp v1 v2)
						then e2'
						else raise Eject
				end
			end
			) s1 (enum s2) in
			match LitlEnumerator.next e2 with
				None -> true
			| _ -> false
		with Eject -> false
	end

	(* val choose : 'a t -> index * 'a *)
	let choose = function
		BTM.Empty -> raise Not_found
	| BTM.Node(_, k, v, _, _) -> (k, v)

	let rec join l key value r = begin 
	  match (l, r) with
	    BTM.Empty, _ -> set key value r
	  | _, BTM.Empty -> set key value l
	  | (BTM.Node(ll, lv, ld, lr, lh), BTM.Node(rl, rv, rd, rr, rh)) ->
	      if lh > rh + 2 then bal ll lv ld (join lr key value r) else
	      if rh > lh + 2 then bal (join l key value rl) rv rd rr else
	      create l key value r
	end

	(* val split : index -> 'a t -> 'a t * 'a option * 'a t *)
	let rec split key t = begin 
		match t with
	    BTM.Empty ->
	      (BTM.Empty, None, BTM.Empty)
	  | BTM.Node(l, k, v, r, _) ->
	      let c = I.compare key k in
	      if c = 0 then (l, Some v, r)
	      else if c < 0 then
	        let (ll, pres, rl) = split key l in (ll, pres, join rl k v r)
	      else
	        let (lr, pres, rr) = split key r in (join l k v lr, pres, rr)
	end

	(* val split_left : index -> 'a t -> 'a t * 'a option *)
	let rec split_left key t = begin 
		match t with
	    BTM.Empty ->
	      (BTM.Empty, None)
	  | BTM.Node(l, k, v, r, _) ->
	      let c = I.compare key k in
	      if c = 0 then (l, Some v)
	      else if c < 0 then
	        let (ll, pres) = split_left key l in (ll, pres)
	      else
	        let (lr, pres) = split_left key r in (join l k v lr, pres)
	end

	(* val split_right : index -> 'a t -> 'a option * 'a t *)
	let rec split_right key t = begin 
		match t with
	    BTM.Empty ->
	      (None, BTM.Empty)
	  | BTM.Node(l, k, v, r, _) ->
	      let c = I.compare key k in
	      if c = 0 then (Some v, r)
	      else if c < 0 then
	        let (pres, rl) = split_right key l in (pres, join rl k v r)
	      else
	        let (pres, rr) = split_right key r in (pres, rr)
	end

end