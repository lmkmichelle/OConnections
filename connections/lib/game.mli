val guessed_words_init : Word.t array
(** Initializes a Word.t array of guessed words, which all have value {word = "empty"; 
    category = "empty"} at initialization. The array will always have size 16. *)

val random_num_list : int array
(** Generates a random integer than can be used to select words from the
    dictionary. *)

val const : string -> int array -> Category.t list
(** [const custom random_num_list] creates a list of four categories, all from
    the corresponding color if custom is ["yellow", "green", "blue", "purple"]
    or one from each from a random game if custom is ["random"] or just all four
    from a random game if custom is anything else. [random_num_list] is the
    value [random_num_list] from game.mli *)

val shuffle : 'a array -> unit
(** [shuffle a] shuffles array [a]'s ordering such that the elements are put
    randomly into new indices. *)

val array_eliminate : Word.t array -> Word.t array -> Word.t array
(** [array_eliminate l1 l2] returns an array that contains all elements from
    [l1] that are not in [l2]. Requires [l2] to be a list of Word.t that has a
    size equal to or less than [l1]. [l2] may contain empty words, as described
    in the documentation for [guessed_words_init]. *)

val update_words_array : Word.t array -> Word.t array -> Word.t array
(** [update_words_array words_array guessed_word] returns a Word.t array such
    that all non-empty words from guessed_word are in the front of the array,
    and the remaining words from words_array are appended to the end. *)

val check_win : Word.t array -> bool
(** [check_win guessed_words] returns true if the player has won; that is, the
    [guessed_words] array contains no empty words (that is, all words have been
    guessed), and false otherwise. *)

val color_match : string -> ANSITerminal.style
(** [color_match i] takes string [i], which represents the difficulty of a
   category and returns the corresponding ANSITerminal.style for it to be
   printed in the right color *)

val category_find : Word.t -> Category.t list -> Category.t
(** [category_find e l] searches through a list [l] of categories to find which
   one contains element [e] *)

val contradiction_eliminator : Category.t list -> Category.t list
(** [contradiction_eliminator l] searches through list [l], performing numerous
   checks on the category names and items in it, changing them randomly until
   there is no repetition of labels. *)
