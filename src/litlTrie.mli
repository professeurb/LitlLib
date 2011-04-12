(************************************************************************
*  Trie.mli
*  
*
*  Created by Olivier Brunet on 15 Jul 2010.
*  Copyright (c) 2010 B.W.C. Computing. All rights reserved.
************************************************************************)

type 'a enum = 'a LitlEnumerator.enum

module type SET = LitlPervasives.SET

module type MAP = LitlPervasives.MAP

module type TRIE = sig 
  type pre_elt

  type 'a map

	type t = { 
		present : bool ;
		next : t map
	}

	include SET with type t := t
	
	val mem_enum : pre_elt enum -> t -> bool
	val add_enum : pre_elt enum -> t -> t
  val remove_enum : pre_elt enum -> t -> t
end

module Make (M : MAP) : TRIE with
  type pre_elt = M.index and
	type elt = M.index list and
	type 'a map = 'a M.t
