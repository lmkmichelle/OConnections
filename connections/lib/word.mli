type t = {
  mutable word : string;
  mutable category : string;
}

val is_empty : t -> bool

val make : string -> string -> t
(**makes a t from strings [s1] and [s2] which are provided as input, with s1
   being the word and s2 being the category*)

val test_word : t
val test_word2 : t
val test_word3 : t

val print_word : t -> unit
(**prints words to be used for debugging purposes*)
