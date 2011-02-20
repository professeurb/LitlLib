(************************************************************************
*
*  litl.mli
*  
*
*  Created by Olivier Brunet on 2 Sep 2010.
*  Copyright (c) 2010 B.W.C. Computing. All rights reserved.
*
************************************************************************)

module Zoom : sig 
  type 'a t = Zero | One of 'a | More

  val bind : ('a -> 'b) -> 'a t -> 'b t
end

type 'a enum
type 'a zoom = 'a Zoom.t

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

module Core : sig 
  module BinaryTree : sig 
    type 'a t =
		    Empty
		  | Node of 'a t * 'a * 'a t * int

		val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
		val iter : ('a -> unit) -> 'a t -> unit

		val choose : 'a t -> 'a
		val min : 'a t -> 'a
		val max : 'a t -> 'a
  end
  
  module BinaryTreeMap : sig 
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
	end

  module Enumerator : sig 
		class type ['a] enumerator = object 
			('enum)
			method next : ('a * 'enum) option
			method fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b
			method iter : ('a -> unit) -> unit
			method skip_until : ('a -> bool) -> ('a * 'enum) option
			method memoized : bool
		end
	
		val empty : 'a enum
		val from_list : 'a list -> 'a enum
		val from_generator : ('a -> ('a * 'b) option) -> 'a -> 'b enum
		val from_unit_generator : (unit -> 'a option) -> 'a enum
		val from_enumerator : 'a enumerator -> 'a enum
		val from_binary_tree : 'a BinaryTree.t -> 'a enum
		val from_binary_tree_map : ('a, 'b) BinaryTreeMap.t -> ('a * 'b) enum
		val from_binary_tree_map_keys : ('a, 'b) BinaryTreeMap.t -> 'a enum
		val from_binary_tree_map_values : ('a, 'b) BinaryTreeMap.t -> 'b enum
		val from_stream : 'a Stream.t -> 'a enum
		val from_once : (unit -> ('a * 'a enum) option) -> 'a enum
		val cons : 'a -> 'a enum -> 'a enum
	
		val next : 'a enum -> ('a * 'a enum) option
		val fold : ('a -> 'b -> 'b) -> 'a enum -> 'b -> 'b
		val iter : ('a -> unit) -> 'a enum -> unit
	
		val map : ('a -> 'b) -> 'a enum -> 'b enum
		val map_with_aux :
			('a -> 'b -> ('c * 'b) option) -> 'a enum -> 'b -> 'c enum
		val map_opt : ('a -> 'b option) -> 'a enum -> 'b enum
		val map_opt_with_aux :
			('a -> 'b -> ('c option * 'b) option) -> 'a enum -> 'b -> 'c enum
		val filter : ('a -> bool) -> 'a enum -> 'a enum
		val concat : 'a enum -> 'a enum -> 'a enum
		val expand : ('a -> 'b enum) -> 'a enum -> 'b enum
		val expand_with_aux :
			('a -> 'c -> 'b enum * 'c) -> 'a enum -> 'c -> 'b enum
		val memo : 'a enum -> 'a enum
	
		val range : int -> int -> int enum
		val counter : int enum
		val trim : int -> 'a enum -> 'a enum
		val to_list : 'a enum -> 'a list	
	end
end

module Set (Ord : ORDERED_TYPE) : SET
with
  type elt = Ord.t

module Map (I : ORDERED_TYPE) : MAP
with
	type index = I.t

module type MULTI_MAP = sig 
	type key
	type value
	type set
	include SET
	
	val add_to : key -> value -> t -> t
	val get_values : key -> t -> set
end

module MultiMap (M : MAP) (S : SET) : MULTI_MAP 
with
  type key = M.index and
  type value = S.elt and
  type elt = M.index * S.elt and
  type set = S.t and
  type t = S.t M.t

module type TRIE = sig 
  include SET
  type pre_elt

	val add_enum : pre_elt enum -> t -> t
end

module Trie (M : MAP) : TRIE with
  type pre_elt = M.index and
	type elt = M.index list

module type TRIE_MAP = sig 
	include MAP

  type index_elt

	val extract : index -> 'a t -> 'a enum
	val extract_enum : index_elt enum -> 'a t -> 'a enum
end

module TrieMap (M : MAP) : TRIE_MAP with
  type index_elt = M.index and
	type index = M.index list
