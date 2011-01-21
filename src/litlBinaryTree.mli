(************************************************************************
*  BinaryTree.mli
*  
*
*  Created by Olivier Brunet on 13 Apr 2010.
*  Copyright (c) 2010 B.W.C. Computing. All rights reserved.
************************************************************************)

type 'a t =
    Empty
  | Node of 'a t * 'a * 'a t * int

val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val iter : ('a -> unit) -> 'a t -> unit

val choose : 'a t -> 'a
val min : 'a t -> 'a
val max : 'a t -> 'a
