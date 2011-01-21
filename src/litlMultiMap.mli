(************************************************************************
*  Multimap.mli
*  
*
*  Created by Olivier Brunet on 8 Jun 2010.
*  Copyright (c) 2010 B.W.C. Computing. All rights reserved.
************************************************************************)

type 'a enum = 'a LitlEnumerator.enum

module type SET = LitlPervasives.SET

module type MAP = LitlPervasives.MAP

module type MULTI_MAP = sig 
	type key
	type value
	type set
	include SET
	
	val add_to : key -> value -> t -> t
	val get_values : key -> t -> set
end

module Make (M : MAP) (S : SET) : MULTI_MAP with
  type key = M.index and
  type value = S.elt and
	type elt = M.index * S.elt and
  type set = S.t and
	type t = S.t M.t
