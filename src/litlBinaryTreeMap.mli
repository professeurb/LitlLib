(************************************************************************
*  BinaryTreeMap.mli
*  
*
*  Created by Olivier Brunet on  30 Apr 2010.
*  Copyright (c) 2010 B.W.C. Computing. All rights reserved.
************************************************************************)

type ('a, 'b) t =
	Empty
| Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t * int

val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
val fold_keys : ('a -> 'b -> 'b) -> ('a, 'c) t -> 'b -> 'b
val fold_values : ('a -> 'b -> 'b) -> ('c, 'a) t -> 'b -> 'b
val fold_elts : ('a * 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c

val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
val iter_keys : ('a -> unit) -> ('a, 'b) t -> unit
val iter_values : ('a -> unit) -> ('b, 'a) t -> unit
val iter_elts : ('a * 'b -> unit) -> ('a, 'b) t -> unit

val choose : ('a, 'b) t -> 'a * 'b
val choose_key : ('a, 'b) t -> 'a

val min : ('a, 'b) t -> 'a * 'b
val max : ('a, 'b) t -> 'a * 'b
