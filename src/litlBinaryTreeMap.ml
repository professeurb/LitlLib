(************************************************************************
*
*  BinaryTreeMap.ml
*  
*
*  Created by Olivier Brunet on 30 Apr 2010.
*  Copyright (c) 2010 B.W.C. Computing. All rights reserved.
*
************************************************************************)

type ('a, 'b) t =
	Empty
| Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t * int 

let rec fold f bt res = begin 
	match bt with
		Empty -> res
	| Node (l, k, v, r, _) -> fold f r (f k v (fold f l res))
end

let rec fold_keys f bt res = begin 
	match bt with
		Empty -> res
	| Node (l, k, v, r, _) -> fold_keys f r (f k (fold_keys f l res))
end

let rec fold_values f bt res = begin 
	match bt with
		Empty -> res
	| Node (l, k, v, r, _) -> fold_values f r (f v (fold_values f l res))
end

let rec fold_elts f bt res = begin 
	match bt with
		Empty -> res
	| Node (l, k, v, r, _) -> fold_elts f r (f (k, v) (fold_elts f l res))
end

let rec iter (f : 'a -> 'b -> unit) bt = begin 
	match bt with
		Empty -> ()
	| Node (l, k, v, r, _) -> begin 
			iter f l ;
			f k v ;
			iter f r
	end
end

let rec iter_keys (f : 'a -> unit) bt = begin 
	match bt with
		Empty -> ()
	| Node (l, k, v, r, _) -> begin 
			iter_keys f l ;
			f k ;
			iter_keys f r
	end
end

let rec iter_values (f : 'a -> unit) bt = begin 
	match bt with
		Empty -> ()
	| Node (l, k, v, r, _) -> begin 
			iter_values f l ;
			f v ;
			iter_values f r
	end
end

let rec iter_elts (f : 'a * 'b -> unit) bt = begin 
	match bt with
		Empty -> ()
	| Node (l, k, v, r, _) -> begin 
			iter_elts f l ;
			f (k, v) ;
			iter_elts f r
	end
end

let choose t = begin 
	match t with
		Empty -> raise Not_found
	| Node (_, k, v, _, _) -> (k, v)
end

let choose_key t = begin 
	match t with
		Empty -> raise Not_found
	| Node (_, k, _, _, _) -> k
end

let rec min t = begin 
	match t with
		Empty -> raise Not_found
	| Node (Empty, k, v, _, _) -> (k, v)
	| Node (l, _, _, _, _) -> min l
end

let rec max t = begin 
	match t with
		Empty -> raise Not_found
	| Node (_, k, v, Empty, _) -> (k, v)
	| Node (_, _, _, r, _) -> max r
end
