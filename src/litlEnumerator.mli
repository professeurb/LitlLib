type 'a enum

class type ['a] enumerator = object ('enum)
	method next : ('a * 'enum) option
	method fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b
	method iter : ('a -> unit) -> unit
	method skip_until : ('a -> bool) -> ('a * 'enum) option
	method memoized : bool
end

val empty : 'a enum
val from_list : 'a list -> 'a enum
val from_generator : ('a -> ('a * 'b) option) -> 'a -> 'b enum
val from_unit_generator : (unit -> 'a option) -> 'a enum
val from_enumerator : 'a enumerator -> 'a enum
val from_binary_tree : 'a LitlBinaryTree.t -> 'a enum
val from_binary_tree_map : ('a, 'b) LitlBinaryTreeMap.t -> ('a * 'b) enum
val from_binary_tree_map_keys : ('a, 'b) LitlBinaryTreeMap.t -> 'a enum
val from_binary_tree_map_values : ('a, 'b) LitlBinaryTreeMap.t -> 'b enum
val from_stream : 'a Stream.t -> 'a enum
val from_once : (unit -> ('a * 'a enum) option) -> 'a enum
val cons : 'a -> 'a enum -> 'a enum

val next : 'a enum -> ('a * 'a enum) option
val fold : ('a -> 'b -> 'b) -> 'a enum -> 'b -> 'b
val iter : ('a -> unit) -> 'a enum -> unit
val skip_until : ('a -> bool) -> 'a enum -> ('a * 'a enum) option

val map : ('a -> 'b) -> 'a enum -> 'b enum
val map_with_aux : ('a -> 'b -> ('c * 'b) option) -> 'a enum -> 'b -> 'c enum
val map_opt : ('a -> 'b option) -> 'a enum -> 'b enum
val map_opt_with_aux :
	('a -> 'b -> ('c option * 'b) option) -> 'a enum -> 'b -> 'c enum
val filter : ('a -> bool) -> 'a enum -> 'a enum
val concat : 'a enum -> 'a enum -> 'a enum
val expand : ('a -> 'b enum) -> 'a enum -> 'b enum
val expand_with_aux : ('a -> 'c -> 'b enum * 'c) -> 'a enum -> 'c -> 'b enum
val memo : 'a enum -> 'a enum

val range : int -> int -> int enum
val counter : int enum
val trim : int -> 'a enum -> 'a enum
val to_list : 'a enum -> 'a list
