type t = {
  name : string;
  items : Word.t array;
  difficulty : int;
}

val make_category_list : string -> int -> t list
(**makes a list of categories with difficulty [diff] from file with the name
   [name], where [name] includes the .txt at the end*)
