(************************************************************************
*  litlZoom.mli
*  
*
*  Created by Olivier Brunet on 18 Dec 2010.
*  Copyright (c) 2010 B.W.C. Computing. All rights reserved.
************************************************************************)

type 'a t = Zero | One of 'a | More

val bind : ('a -> 'b) -> 'a t -> 'b t
