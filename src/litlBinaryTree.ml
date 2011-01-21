(************************************************************************
*
*  BinaryTree.ml
*  
*
*  Created by Olivier Brunet on 13 Apr 2010.
*  Copyright (c) 2010 B.W.C. Computing. All rights reserved.
*
************************************************************************)

(* In this module, we put everything needed for “reading” values from a binary tree. In particular,  *)

type 'a t =
	Empty
| Node of 'a t * 'a * 'a t * int 

let rec fold f bt res = begin 
	match bt with
		Empty -> res
	| Node (l, v, r, _) -> fold f r (f v (fold f l res))
end

let rec iter (f : 'a -> unit) bt = begin 
	match bt with
		Empty -> ()
	| Node (l, v, r, _) -> begin 
			iter f l ;
			f v ;
			iter f r
	end
end

let choose t = begin 
	match t with
		Empty -> raise Not_found
	| Node (_, v, _, _) -> v
end

let rec min t = begin 
	match t with
		Empty -> raise Not_found
	| Node (Empty, v, _, _) -> v
	| Node (l, _, _, _) -> min l
end

let rec max t = begin 
	match t with
		Empty -> raise Not_found
	| Node (_, v, Empty, _) -> v
	| Node (_, _, r, _) -> max r
end
