(************************************************************************
*
*  litlPervasives.ml
*  
*
*  Created by Olivier Brunet on 2 Dec 2010.
*  Copyright (c) 2010 B.W.C. Computing. All rights reserved.
*
************************************************************************)

type 'a zoom = 'a LitlZoom.t

type 'a enum = 'a LitlEnum.enum

module type ORDERED_TYPE = sig 
	type t
	val compare : t -> t -> int
	val equal : t -> t -> bool
end

module type SET = sig 
	type elt
	type t
	
	val empty: t
	val is_empty: t -> bool
	
	val height : t -> int
	val zoom : t -> elt zoom
	
	val mem: elt -> t -> bool
	
	val add: elt -> t -> t
	val remove : elt -> t -> t
  val change : (bool -> bool) -> elt -> t -> t
  val change_return : (bool -> bool * 'a) -> elt -> t -> t * 'a
	
	val singleton: elt -> t
	
	val union: t -> t -> t
	val inter: t -> t -> t
	val diff: t -> t -> t
	
	val subset : t -> t -> bool
	
	val enum : t -> elt enum
	val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
	val iter: (elt -> unit) -> t -> unit
	
	val for_all: (elt -> bool) -> t -> bool
	val exists: (elt -> bool) -> t -> bool
	
	val compare: t -> t -> int
	val equal: t -> t -> bool
	
	val min: t -> elt
	val max: t -> elt
	val choose: t -> elt
		
	val split : elt -> t -> t * bool * t
	val split_left : elt -> t -> t * bool
	val split_right : elt -> t -> bool * t

	val filter : (elt -> bool) -> t -> t
end

module type MAP = sig 
	type index
	type 'a elt = index * 'a
	type 'a t

	val empty : 'a t
	val is_empty : 'a t -> bool

	val height : 'a t -> int
	val zoom : 'a t -> 'a elt zoom

	val find : index -> 'a t -> 'a
	val find_opt : index -> 'a t -> 'a option

	val set : index -> 'a -> 'a t -> 'a t
	val remove : index -> 'a t -> 'a t
	val change : ('a option -> 'a option) -> index -> 'a t -> 'a t
	val change_return :
		('a option -> 'a option * 'b) -> index -> 'a t -> 'a t * 'b

	val has_key : index -> 'a t -> bool

	val enum : 'a t -> 'a elt enum
	val iter : (index -> 'a -> unit) -> 'a t -> unit
	val fold : (index -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

	val for_all: (index -> 'a -> bool) -> 'a t -> bool
	val exists: (index -> 'a -> bool) -> 'a t -> bool

	val keys : 'a t -> index enum
	val values : 'a t -> 'a enum

	val map : ('a -> 'b) -> 'a t -> 'b t
	val map_opt : ('a -> 'b option) -> 'a t -> 'b t
	val map_with_key : (index -> 'a -> 'b) -> 'a t -> 'b t
	val map_opt_with_key : (index -> 'a -> 'b option) -> 'a t -> 'b t

	val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
	val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

	(* Elements retrieval *)
	val choose : 'a t -> 'a elt
	val min : 'a t -> 'a elt
	val max : 'a t -> 'a elt

	val split : index -> 'a t -> 'a t * 'a option * 'a t
	val split_left : index -> 'a t -> 'a t * 'a option
	val split_right : index -> 'a t -> 'a option * 'a t 
end
