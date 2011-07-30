(************************************************************************
*
*  litl.ml
*  
*
*  Created by Olivier Brunet on 2 Sep 2010.
*  Copyright (c) 2010 B.W.C. Computing. All rights reserved.
*
************************************************************************)

type 'a enum = 'a LitlEnum.enum
type 'a zoom = 'a LitlZoom.t

module Zoom = LitlZoom

module type ORDERED_TYPE = LitlPervasives.ORDERED_TYPE
module type SET = LitlPervasives.SET
module type MAP = LitlPervasives.MAP

module Core = struct 
  module BinaryTree = LitlBinaryTree
	module BinaryTreeMap = LitlBinaryTreeMap
	module Enumerator = LitlEnum
end

module Set (Ord : ORDERED_TYPE) : SET
with
  type elt = Ord.t
= LitlAVLSet.Make (Ord)

module Map (I : ORDERED_TYPE) : MAP
with
	type index = I.t
= LitlAVLMap.Make (I)

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
= LitlMultiMap.Make (M) (S)

module type TRIE = sig 
  type atom
  type 'a map

	type t = { 
	  present : bool ;
	  next : t map
	}

	include SET with type t := t
	
	val mem_next : ('a -> (atom * 'a) option) -> 'a -> t -> bool
	val add_next : ('a -> (atom * 'a) option) -> 'a -> t -> t
  val remove_next : ('a -> (atom * 'a) option) -> 'a -> t -> t
end

module Trie (M : MAP) : TRIE with
  type atom = M.index and
  type elt = M.index list and
  type 'a map = 'a M.t
= LitlTrie.Make (M)

module type TRIE_MAP = sig 
	include MAP

  type index_elt

	val extract : index -> 'a t -> 'a enum
	val extract_enum : index_elt enum -> 'a t -> 'a enum
end

module TrieMap (M : MAP) : TRIE_MAP with
  type index_elt = M.index and
	type index = M.index list
= LitlTrieMap.Make (M)