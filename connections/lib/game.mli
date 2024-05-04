(** Initializes a Word.t array of guessed words, which all have value {word = "empty"; 
    category = "empty"} at initialization. The array will always have size 16. *)
val guessed_words_init : Word.t array

(** Generates a random integer than can be used to select words from the dictionary. *)
val random_num : int

(** Creates a list of four categories, one from each difficulty level. *)
val const: int -> Category.t list

(** [shuffle a] shuffles array [a]'s ordering such that the elements are put randomly 
into new indices. *)
val shuffle: 'a array -> unit

(** [array_eliminate l1 l2] returns an array that contains all elements from [l1] that 
    are not in [l2]. Requires [l2] to be a list of Word.t that has a size equal to or
    less than [l1]. [l2] may contain empty words, as described in the documentation for
    [guessed_words_init]. *)
val array_eliminate: Word.t array -> Word.t array -> Word.t array

(** [update_words_array words_array guessed_word] returns a Word.t array such that all
    non-empty words from guessed_word are in the front of the array, and the remaining 
    words from words_array are appended to the end. *)
val update_words_array: Word.t array -> Word.t array -> Word.t array

(** [check_win guessed_words] returns true if the player has won; that is, the 
    [guessed_words] array contains no empty words (that is, all words have been
guessed), and false otherwise. *)
val check_win: Word.t array -> bool

