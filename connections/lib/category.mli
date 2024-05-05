type t = {
  name : string;
  hint : string;
  items : Word.t array;
  difficulty : string;
}

val contains : Word.t -> t -> bool
(**searches through the items of c to check if any of them are equal to 
    value e, returns true if one is*)
val make_category_list : string -> string -> t list
(**makes a list of categories with difficulty [diff] from file with the name
   [name], where [name] includes the .txt at the end*)
val make : string -> string -> Word.t array -> string -> t
(**makes a value of type t with name s1, hint s2, items i, and difficulty s3*)
val diff : t -> string
(**returns the difficulty of a value of type t*)
val name : t -> string
(**returns the name of a value of type t*)