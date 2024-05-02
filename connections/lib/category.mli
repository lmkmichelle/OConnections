type t = {
  name : string;
  hint : string;
  items : Word.t array;
  difficulty : string;
}

val make_category_list : string -> string -> t list
(**makes a list of categories with difficulty [diff] from file with the name
   [name], where [name] includes the .txt at the end*)
