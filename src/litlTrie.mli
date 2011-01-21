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
	include SET
	
	val add_enum : pre_elt enum -> t -> t
end

module Make (M : MAP) : TRIE with
  type pre_elt = M.index and
	type elt = M.index list
