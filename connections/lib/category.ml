type t = {
  name : string;
  items : Word.t array;
  difficulty : int;
}

let list_from_file name = BatList.of_enum (BatFile.lines_of ("data/" ^ name))

let rec separate_string_list = function
  | [] -> []
  | h :: t ->
      Array.of_list (String.split_on_char ',' h) :: separate_string_list t

let rec category_list_of_string_array_list diff = function
  | [] -> []
  | h :: t ->
      {
        name = h.(0);
        items = Array.init 4 (fun x -> Word.make h.(x) h.(0));
        difficulty = diff;
      }
      :: category_list_of_string_array_list diff t

let make_category_list file diff =
  category_list_of_string_array_list diff
    (separate_string_list (list_from_file file))
