(************************************************************************
*  LitlMap.mli
*  
*
*  Created by Olivier Brunet on 10 May 2010.
*  Copyright (c) 2010 B.W.C. Computing. All rights reserved.
************************************************************************)

type 'a enum = 'a LitlEnumerator.enum

module type OrderedType = LitlPervasives.ORDERED_TYPE

module type Map = LitlPervasives.MAP

module Make (I : OrderedType) : Map with
	type index = I.t
