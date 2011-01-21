(************************************************************************
*  LitlSet.mli
*  
*
*  Created by Olivier Brunet on 17 Apr 2010.
*  Copyright (c) 2010 B.W.C. Computing. All rights reserved.
************************************************************************)

type 'a enum = 'a LitlEnumerator.enum

module type OrderedType = LitlPervasives.ORDERED_TYPE

module type Set = LitlPervasives.SET

module Make(Ord: OrderedType) : Set with
	type elt = Ord.t
