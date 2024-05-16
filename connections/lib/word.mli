type t = {
  mutable word : string;
  mutable category : string;
}
(** The type of a word. [word] is the string representing the word itself, and
    [category] represents the category that this word belongs to. *)

val is_empty : t -> bool
(** [is_empty word] returns true if [word].word is an empty string, and false otherwise *)
  
val make : string -> string -> t
(** [make word category] makes a Word where [word] is the word and [category] 
    is the category it belongs to. *)

val test_word : t
val test_word2 : t
val test_word3 : t

val print_word : t -> unit
(**prints words to be used for debugging purposes*)
