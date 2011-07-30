(************************************************************************
*
*  MapToSet.ml
*  
*
*  Created by Olivier Brunet on 8 Jun 2010.
*  Copyright (c) 2010 B.W.C. Computing. All rights reserved.
*
************************************************************************)

type 'a enum = 'a LitlEnum.enum

module type SET = LitlPervasives.SET

module type MAP = LitlPervasives.MAP

module type MULTI_MAP = sig 
	type key
	type value
	type set
	include SET
	
	val add_to : key -> value -> t -> t
	val get_values : key -> t -> set
end

module Make (M : MAP) (S : SET) : MULTI_MAP with
  type key = M.index and
  type value = S.elt and
  type elt = M.index * S.elt and
  type set = S.t and
  type t = S.t M.t
= struct 
	type key = M.index
	type value = S.elt
	type elt = key * value
	type set = S.t
	type t = set M.t
		
	let empty = M.empty
	let is_empty t = M.is_empty t

	(* val height : 'a t -> int *)
	let height t = begin 
		match M.height t with
			0 -> 0
		| 1 -> let (_, c) = M.choose t in S.height c
		| n -> n
	end

  let zoom t = begin 
    match M.zoom t with
      LitlZoom.One (key, values) -> begin 
	      match S.zoom values with
	        LitlZoom.One value -> LitlZoom.One (key, value)
	      | LitlZoom.Zero -> LitlZoom.Zero
	      | LitlZoom.More -> LitlZoom.More
	    end
    | LitlZoom.Zero -> LitlZoom.Zero
    | LitlZoom.More -> LitlZoom.More
  end

	(* val mem : elt -> t -> bool *)
	let mem (key, value) t = begin 
		try
			S.mem value (M.find key t)
		with Not_found -> false
	end
	
	(* val add : elt -> t -> t *)
	let add (key, value) t = begin 
		M.change (function
				None -> Some (S.singleton value) 
			| Some c -> Some (S.add value c)
		) key t
	end

  (* val add_to : key -> value -> t -> t *)
	let add_to key value t = begin 
		M.change (function
				None -> Some (S.singleton value) 
			| Some c -> Some (S.add value c)
		) key t
	end

	(* val remove : elt -> t -> t *)
	let remove (key, value) t = begin 
		M.change (function
				None -> None
			| Some c -> begin 
					let c' = S.remove value c in
					if S.is_empty c' then None else Some c'
			end
		) key t
	end

  (* val change : (bool -> bool) -> elt -> t -> t *)
  let change f (key, value) t = begin 
    M.change (function
	    None -> if f false then Some (S.singleton value) else None
		| Some c -> begin 
        let c' = S.change f value c in
        if S.is_empty c' then None else Some c'
	    end
	  ) key t
  end

  let change_return f (key, value) t = begin
	    M.change_return (function
		    None -> begin 
		      let (flag, result) = f false in
		      if flag then (Some (S.singleton value), result) else (None, result)
        end
			| Some c -> begin 
	        let (c', result) = S.change_return f value c in
	        if S.is_empty c' then (None, result) else (Some c', result)
		    end
		  ) key t
	  end

	(* val singleton: elt -> t *)
	let singleton (key, value) = begin 
		M.set key (S.singleton value) M.empty
	end
	
	(* val union: t -> t -> t *)
	let union t1 t2 = begin 
		M.fold (fun key c t ->
				M.change (function 
					None -> Some c
				| Some c' -> Some (S.union c c')
			) key t
		) t1 t2
	end

	(* val inter: t -> t -> t *)
	let inter t1 t2 = begin 
		M.map_opt_with_key (
			fun key c -> begin 
				match M.find_opt key t2 with
					None -> None
				| Some c' -> let c'' = S.inter c c' in
						if S.is_empty c'' then None else Some c''
			end
		) t1
	end

	(* val diff: t -> t -> t *)
	let diff t1 t2 = begin 
		M.map_opt_with_key (fun key c -> begin 
				match M.find_opt key t2 with
					None -> Some c
				| Some c' -> let c'' = S.diff c c' in
						if S.is_empty c'' then None else Some c''
			end
		) t1
	end
	
	(* val subset : t -> t -> bool *)
	let subset t1 t2 = begin 
		try
			M.for_all (fun key c -> S.subset c (M.find key t2)) t1
		with Not_found -> false
	end
	
	(* val enum : t -> elt enum *)
	let enum t = begin 
		LitlEnum.expand (
		  fun (key, c) -> LitlEnum.map (
			  fun value -> (key, value)) (S.enum c)
		) (M.enum t)
	end
	
	(* val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a *)
	let fold f t accu = begin 
		M.fold (
	    fun key c a -> S.fold (
		    fun value a -> f (key, value) a
		  ) c a
		) t accu
	end

	(* val iter: (elt -> unit) -> t -> unit *)
	let iter f t = begin 
		M.iter
			(fun key c -> S.iter (fun value -> f (key, value)) c) t
	end
	
	(* val for_all: (elt -> bool) -> t -> bool *)
	let for_all p t = begin 
		M.for_all (fun key c -> S.for_all (fun value -> p (key, value)) c) t
	end
	
	(* val exists: (elt -> bool) -> t -> bool *)
	let exists p t = begin 
		M.exists (fun key c -> S.exists (fun value -> p (key, value)) c) t
	end
	
	(* val compare: t -> t -> int *)
	let compare t1 t2 = M.compare (S.compare) t1 t2

	(* val equal: t -> t -> bool *)
	let equal t1 t2 = M.equal (S.equal) t1 t2
	
	(* val min: t -> elt *)
	let min t = begin 
		let (key, c) = M.min t in
		(key, S.min c)
	end

	(* val max: t -> elt *)
	let max t = begin 
		let (key, c) = M.max t in
		(key, S.max c)
	end

	(* val choose: t -> elt *)
	let choose t = begin 
		let (key, c) = M.choose t in
		(key, S.choose c)
	end

	(* val split : elt -> t -> t * bool * t *)
	let split (key, value) t = begin 
		let (tl, c_opt, tr) = M.split key t in
		let c = match c_opt with None -> S.empty | Some c' -> c' in
		let (cl, pres, cr) = S.split value c in
		let l = begin 
			if S.is_empty cl then tl else M.set key cl tl
		end
		and r = begin
			if S.is_empty cr then tr else M.set key cr tr
		end in
		(l, pres, r)
	end

	(* val split_left : elt -> t -> t * bool *)
	let split_left (key, value) t = begin 
  	let (tl, c_opt) = M.split_left key t in
  	let c = match c_opt with None -> S.empty | Some c' -> c' in
  	let (cl, pres) = S.split_left value c in
  	let l = begin 
  		if S.is_empty cl then tl else M.set key cl tl
  	end in
  	(l, pres)
  end

	(* val split_right : elt -> t -> bool * t *)
	let split_right (key, value) t = begin 
		let (c_opt, tr) = M.split_right key t in
		let c = match c_opt with None -> S.empty | Some c' -> c' in
		let (pres, cr) = S.split_right value c in
		let r = begin
			if S.is_empty cr then tr else M.set key cr tr
		end in
		(pres, r)
	end

	(* val filter : (elt -> bool) -> t -> t *)
	let filter f t = begin 
		M.map_opt_with_key (fun key c -> 
			let c' = S.filter (fun value -> f (key, value)) c in
			if S.is_empty c' then None else Some c'
		) t
	end

	let get_values key t = begin 
		match M.find_opt key t with
			None -> S.empty
		| Some set -> set
	end

  let zoom t = begin 
    match M.zoom t with
      LitlZoom.One (key, values) -> begin 
	      match S.zoom values with
	        LitlZoom.One value -> LitlZoom.One (key, value)
	      | LitlZoom.Zero -> LitlZoom.Zero
	      | LitlZoom.More -> LitlZoom.More
	    end
    | LitlZoom.Zero -> LitlZoom.Zero
    | LitlZoom.More -> LitlZoom.More
  end
end
