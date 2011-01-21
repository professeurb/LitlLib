(************************************************************************
*
*  litlZoom.ml
*  
*
*  Created by Olivier Brunet on 18 Dec 2010.
*  Copyright (c) 2010 B.W.C. Computing. All rights reserved.
*
************************************************************************)

type 'a t = Zero | One of 'a | More

let bind f z = begin
  match z with
    Zero -> Zero
  | One elt -> One (f elt)
  | More -> More
end
