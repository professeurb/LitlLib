(************************************************************************
*  Trie.mli
*  
*
*  Created by Olivier Brunet on 15 Jul 2010.
*  Copyright (c) 2010 B.W.C. Computing. All rights reserved.
************************************************************************)

type 'a enum = 'a LitlEnum.enum

module type SET = LitlPervasives.SET

module type MAP = LitlPervasives.MAP

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

module Make (M : MAP) : TRIE with
  type atom = M.index and
	type elt = M.index list and
	type 'a map = 'a M.t
