type t = {
  name : string;
  hint : string;
  items : Word.t array;
  difficulty : string;
}
(** The type for a category. [name] represents the name of a specific category
    (i.e. "fruits"). [hint] represents the unique hint for a specific category.
    [items] is a Word.t array containing all Word.t that is in this specific
    category. [difficulty] represents the difficulty of this category, coded by
    colors. *)

val contains : Word.t -> t -> bool
(** [contains e c] Searches through the items of [c] to check if any of them are
    equal to value [e], returns true if one is. *)

val make_category_list : string -> string -> t list
(** [make_category_list file diff] makes a list of categories with difficulty
    [diff] from file with the name [name], where [name] includes the .txt at the
    end*)

val make : string -> string -> Word.t array -> string -> t
(** [make name hint i duff] makes a value of type t with name [name], hint
    [hint], items [i], and difficulty [diff] *)

val diff : t -> string
(** [diff c] returns the difficulty of the category [c] *)

val name : t -> string
(** [name c] returns the name of the category [c] *)

val items : t -> Word.t array
(** [items c] returns an array containing all of the Words in category [c] *)

val print_category : t -> unit
(**prints categories to be used for debugging purposes*)

val test_category : t
(**A category for testing with no items and all other values set to ["string"]*)

val test_category2 : t
(**A category for testing with the only item as [Word.test_word] and all other
   values as ["string"]*)

val test_category3 : t
(**A category for testing with items as [Word.test_word], [Word.test_word2], and
   [Word.test_word3] amd all other categories as ["string"]*)

val test_category4 : t
(**A category for testing with name as ["abcd"], hint as ["hi"], items the same
   as those of test_category3, and difficulty as ["4"]*)
