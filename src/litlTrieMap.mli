(************************************************************************
*  TrieMap.mli
*  
*
*  Created by Olivier Brunet on 9 Jun 2010.
*  Copyright (c) 2010 B.W.C. Computing. All rights reserved.
************************************************************************)

type 'a enum = 'a LitlEnum.enum

module type MAP = LitlPervasives.MAP

module type TRIE_MAP = sig 
	include MAP

  type index_elt

	val extract : index -> 'a t -> 'a enum
	val extract_enum : index_elt enum -> 'a t -> 'a enum
end

module Make (M : MAP) : TRIE_MAP with
  type index_elt = M.index and
	type index = M.index list
