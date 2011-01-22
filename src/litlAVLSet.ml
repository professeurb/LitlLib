(************************************************************************
*
*  LitlSet.ml
*  
*
*  Created by Olivier Brunet on 17 Apr 2010.
*  Copyright (c) 2010 B.W.C. Computing. All rights reserved.
*
************************************************************************)

(*

#load "BinaryTree.cmo" ;;
#load "LitlEnumerator.cmo" ;;

module Ord = struct type t = int let compare = compare end

*)

type 'a enum = 'a LitlEnumerator.enum

module type OrderedType = LitlPervasives.ORDERED_TYPE

module type Set = LitlPervasives.SET

module Make(Ord: OrderedType) : Set with
	type elt = Ord.t
= struct 
	module BT = LitlBinaryTree

  type elt = Ord.t
	type t = elt BT.t

	exception Eject	

  (* Sets are represented by balanced binary trees (the heights of the
     children differ by at most 2 *)

  let height = function
      BT.Empty -> 0
    | BT.Node(_, _, _, h) -> h

  let zoom = function
    BT.Empty -> LitlZoom.Zero
  | BT.Node (BT.Empty, v, BT.Empty, _) -> LitlZoom.One v
  | _ -> LitlZoom.More

	(* AVL Operations *)

  (* Creates a new node with left son l, value v and right son r.
     We must have all elements of l < v < all elements of r.
     l and r must be balanced and | height l - height r | <= 2.
     Inline expansion of height for better speed. *)

  let create l v r =
    let hl = match l with BT.Empty -> 0 | BT.Node (_,_,_,h) -> h in
    let hr = match r with BT.Empty -> 0 | BT.Node (_,_,_,h) -> h in
    BT.Node (l, v, r, (if hl >= hr then hl + 1 else hr + 1))

  (* Same as create, but performs one step of rebalancing if necessary.
     Assumes l and r balanced and | height l - height r | <= 3.
     Inline expansion of create for better speed in the most frequent case
     where no rebalancing is required. *)

  let bal l v r = begin 
    let hl = begin 
			match l with
				BT.Empty -> 0
			| BT.Node(_,_,_,h) -> h
		end
		and hr = begin 
			match r with
				BT.Empty -> 0
			| BT.Node(_,_,_,h) -> h
		end
		in
    if hl > hr + 2 then begin 
      match l with
        BT.Empty -> invalid_arg "Set.bal"
      | BT.Node(ll, lv, lr, _) ->
          if height ll >= height lr then
            create ll lv (create lr v r)
          else begin 
            match lr with
              BT.Empty -> invalid_arg "Set.bal"
            | BT.Node(lrl, lrv, lrr, _)->
                create (create ll lv lrl) lrv (create lrr v r)
          end
    end
		else if hr > hl + 2 then begin 
      match r with
        BT.Empty -> invalid_arg "Set.bal"
      | BT.Node(rl, rv, rr, _) ->
          if height rr >= height rl then
            create (create l v rl) rv rr
          else begin 
            match rl with
              BT.Empty -> invalid_arg "Set.bal"
            | BT.Node(rll, rlv, rlr, _) ->
                create (create l v rll) rlv (create rlr rv rr)
          end
    end
		else
      BT.Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))
	end

  let rec add x t = begin 
		match t with
      BT.Empty -> BT.Node(BT.Empty, x, BT.Empty, 1)
    | BT.Node(l, v, r, _) as t -> begin 
        let c = Ord.compare x v in
        if c = 0 then t else
        if c < 0 then bal (add x l) v r else bal l v (add x r)
		end
	end

  (* join : Same as create and bal, but no assumptions are made on the
     relative heights of l and r. *)

  let rec join l v r = begin 
    match (l, r) with
      (BT.Empty, _) -> add v r
    | (_, BT.Empty) -> add v l
    | (BT.Node (ll, lv, lr, lh), BT.Node (rl, rv, rr, rh)) ->
        if lh > rh + 2 then bal ll lv (join lr v r) else
        if rh > lh + 2 then bal (join l v rl) rv rr else
        create l v r
	end

  let rec min = function
      BT.Empty -> raise Not_found
    | BT.Node (BT.Empty, v, _, _) -> v
    | BT.Node (l, v, _, _) -> min l

  let rec remove_min = function
      BT.Empty -> invalid_arg "Set.remove_min"
    | BT.Node(BT.Empty, v, r, _) -> r
    | BT.Node(l, v, r, _) -> bal (remove_min l) v r

  let merge t1 t2 = begin 
    match (t1, t2) with
      (BT.Empty, t) -> t
    | (t, BT.Empty) -> t
    | (_, _) -> bal t1 (min t2) (remove_min t2)
	end

  (* concat : Merge two trees l and r into one.
     All elements of l must precede the elements of r.
     No assumption on the heights of l and r. *)

  let concat t1 t2 = begin 
    match (t1, t2) with
      (BT.Empty, t) -> t
    | (t, BT.Empty) -> t
    | (_, _) -> join t1 (min t2) (remove_min t2)
	end
	
	(* Functions *)

  let empty = BT.Empty

  let is_empty = function BT.Empty -> true | _ -> false

  let rec mem x = function
      BT.Empty -> false
    | BT.Node(l, v, r, _) ->
        let c = Ord.compare x v in
        c = 0 || mem x (if c < 0 then l else r)

  let rec remove x t = begin 
		match t with
      BT.Empty -> BT.Empty
    | BT.Node(l, v, r, _) -> begin 
        let c = Ord.compare x v in
        if c = 0 then merge l r else
        if c < 0 then bal (remove x l) v r else bal l v (remove x r)
		end
	end
	
  let rec change f elt t = begin 
    match t with
      BT.Empty -> begin 
        if f false
        then BT.Node(BT.Empty, elt, BT.Empty, 1)
        else BT.Empty
      end
    | BT.Node (l, v, r, _) as t -> begin 
        let c = Ord.compare elt v in
        if c = 0
        then begin 
	        if f true then t else merge l r
        end
        else
	      if c < 0 then bal (change f elt l) v r else bal l v (change f elt r)
    end
  end

  let rec change_return f elt t = begin 
    match t with
      BT.Empty -> begin 
        let (flag, result) = f false in
        if flag
        then (BT.Node(BT.Empty, elt, BT.Empty, 1), result)
        else (BT.Empty, result)
      end
    | BT.Node (l, v, r, _) as t -> begin 
        let c = Ord.compare elt v in
        if c = 0
        then begin 
          let (flag, result) = f true in
	        if flag
	        then (t, result) else (merge l r, result)
        end
        else
	      if c < 0 
	      then begin
		      let (l', result) = change_return f elt l in
		      (bal l' v r, result)
		    end
		    else begin 
		      let (r', result) = change_return f elt r in
		      (bal l v r', result)
		    end
    end
  end

  (* Splitting.  split x s returns a triple (l, present, r) where
      - l is the set of elements of s that are < x
      - r is the set of elements of s that are > x
      - present is false if s contains no element equal to x,
        or true if s contains an element equal to x. *)

  let rec split x = function
      BT.Empty ->
        (BT.Empty, false, BT.Empty)
    | BT.Node(l, v, r, _) ->
        let c = Ord.compare x v in
        if c = 0 then (l, true, r)
        else if c < 0 then
          let (ll, pres, rl) = split x l in (ll, pres, join rl v r)
        else
          let (lr, pres, rr) = split x r in (join l v lr, pres, rr)

  let rec split_right x = function
      BT.Empty ->
        (false, BT.Empty)
    | BT.Node(l, v, r, _) ->
        let c = Ord.compare x v in
        if c = 0 then (true, r)
        else if c < 0 then
          let (pres, rl) = split_right x l in (pres, join rl v r)
        else
          split_right x r

  (* Implementation of the set operations *)

  let singleton x = BT.Node(BT.Empty, x, BT.Empty, 1)

  let rec union s1 s2 = begin 
    match (s1, s2) with
      (BT.Empty, t2) -> t2
    | (t1, BT.Empty) -> t1
    | (BT.Node(l1, v1, r1, h1), BT.Node(l2, v2, r2, h2)) ->
        if h1 >= h2 then
          if h2 = 1 then add v2 s1 else begin
            let (l2, _, r2) = split v1 s2 in
            join (union l1 l2) v1 (union r1 r2)
          end
        else
          if h1 = 1 then add v1 s2 else begin
            let (l1, _, r1) = split v2 s1 in
            join (union l1 l2) v2 (union r1 r2)
          end
	end

  let rec inter s1 s2 =
    match (s1, s2) with
      (BT.Empty, t2) -> BT.Empty
    | (t1, BT.Empty) -> BT.Empty
    | (BT.Node(l1, v1, r1, _), t2) ->
        match split v1 t2 with
          (l2, false, r2) ->
            concat (inter l1 l2) (inter r1 r2)
        | (l2, true, r2) ->
            join (inter l1 l2) v1 (inter r1 r2)

  let rec diff s1 s2 =
    match (s1, s2) with
      (BT.Empty, t2) -> BT.Empty
    | (t1, BT.Empty) -> t1
    | (BT.Node(l1, v1, r1, _), t2) ->
        match split v1 t2 with
          (l2, false, r2) ->
            join (diff l1 l2) v1 (diff r1 r2)
        | (l2, true, r2) ->
            concat (diff l1 l2) (diff r1 r2)

	let enum t = begin 
		LitlEnumerator.from_binary_tree t
	end

	let fold = BT.fold
	let iter = BT.iter
	
	exception Diff of int
	
	let compare s1 s2 = begin 
		try
			let e2 = fold (fun v1 e2 -> begin 
				match LitlEnumerator.next e2 with
					None -> raise (Diff 1)
				| Some (v2, e2') -> begin 
						let c = Ord.compare v1 v2 in
						if c <> 0
						then raise (Diff c)
						else e2'
				end
			end
			) s1 (enum s2) in
			match LitlEnumerator.next e2 with
				None -> 0
			| _ -> -1
		with Diff r -> r
	end
	
  let equal s1 s2 = begin 
    (* compare s1 s2 = 0 *)
		try
			let e2 = fold (fun v1 e2 -> begin 
				match LitlEnumerator.next e2 with
					None -> raise Eject
				| Some (v2, e2') -> begin 
						if Ord.equal v1 v2 then e2' else raise Eject
				end
			end
			) s1 (enum s2) in
			match LitlEnumerator.next e2 with
				None -> true
			| _ -> false
		with Eject -> false
	end
	
  let rec subset s1 s2 = begin 
    match (s1, s2) with
      BT.Empty, _ ->
        true
    | _, BT.Empty ->
        false
    | BT.Node (l1, v1, r1, _), (BT.Node (l2, v2, r2, _) as t2) ->
        let c = Ord.compare v1 v2 in
        if c = 0 then
          subset l1 l2 && subset r1 r2
        else if c < 0 then
          subset (BT.Node (l1, v1, BT.Empty, 0)) l2 && subset r1 t2
        else
          subset (BT.Node (BT.Empty, v1, r1, 0)) r2 && subset l1 t2
	end
		
	let rec filter f s = begin 
		match s with
			BT.Empty -> BT.Empty
		| BT.Node(l, v, r, _) -> begin 
				let l' = filter f l
				and r' = filter f r in
				if f v
				then join l' v r'
				else concat l' r'
		end
	end
	
(*

let chrono f = begin 
	let t1 = Sys.time () in
	let r = f () in
	let t2 = Sys.time () in
	print_float (t2 -. t1) ;
	print_newline () ;
	r
end

Random.self_init ;;

let numbers = begin
	let n_r = ref [] in
	for i = 1 to 100000 do
		n_r := (Random.int 100000) :: ! n_r
	done ;
	! n_r
end ;;

let t_1 =
	chrono (fun () -> List.fold_left (fun t x -> add x t) empty numbers) ;;

(* 0.55 -> 0.60 *)

let t_2 =
	chrono (fun () -> List.fold_left (fun t x ->
		match add_opt x t with
			None -> t
		| Some t' -> t') empty numbers) ;;

chrono (fun () -> let _ = filter (fun x -> x mod 2 = 0) t_1 in ()) ;;


*)
	
	(* let rec iter_gt f x s = begin 
		match s with
			BT.Empty -> ()
		| BT.Node (l, v, r, _) -> 
				if Ord.compare x v >= 0
				then iter_gt f x r (* if v <= x *)
				else begin 
					iter_gt f x l ;
					f v ;
					iter f r
				end (* if v > x *)
	end *)

	(* let rec fold_gt f x s accu = begin 
		match s with
			BT.Empty -> accu
		| BT.Node (l, v, r, _) -> 
				if Ord.compare x v >= 0
				then fold_gt f x r accu
				else fold f r (f v (fold_gt f x l accu))
	end *)

	(* let rec fold_ge f x s accu = begin 
		match s with
			BT.Empty -> accu
		| BT.Node (l, v, r, _) -> 
				if Ord.compare x v > 0
				then fold_gt f x r accu
				else fold f r (f v (fold_gt f x l accu))
	end *)

  let rec for_all p = function
      BT.Empty -> true
    | BT.Node(l, v, r, _) -> p v && for_all p l && for_all p r

  let rec exists p = function
      BT.Empty -> false
    | BT.Node(l, v, r, _) -> p v || exists p l || exists p r

  let rec cardinal = function
      BT.Empty -> 0
    | BT.Node(l, v, r, _) -> cardinal l + 1 + cardinal r

	let rec max = function
      BT.Empty -> raise Not_found
    | BT.Node (_, v, BT.Empty, _) -> v
    | BT.Node (_, v, r, _) -> max r
  
  let choose = function
		BT.Empty -> raise Not_found
	| BT.Node (_, v, _, _) -> v
end
