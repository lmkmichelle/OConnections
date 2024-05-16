open Word

type t = {
  name : string;
  hint : string;
  items : Word.t array;
  difficulty : string;
}

let print_category ctg =
  Printf.printf "{\n";
  Printf.printf " name: \"%s\";\n" ctg.name;
  Printf.printf " hint: \"%s\";\n" ctg.hint;
  Printf.printf " items: [|";
  Array.iteri
    (fun i word ->
      if i > 0 then Printf.printf "; ";
      print_word word)
    ctg.items;
  Printf.printf "|];\n";
  Printf.printf " difficulty : \"%s\;\n" ctg.difficulty;
  Printf.printf "}\n"

let contains e c =
  let x = ref false in
  for i = 0 to Array.length c.items - 1 do
    if c.items.(i) = e then x := true
    else
      let () = () in
      ()
  done;
  !x

let list_from_file name = BatList.of_enum (BatFile.lines_of ("files/" ^ name))

let rec separate_string_list = function
  | [] -> []
  | h :: t ->
      Array.of_list (String.split_on_char ',' h) :: separate_string_list t

let rec category_list_of_string_array_list diff = function
  | [] -> []
  | h :: t ->
      {
        name = h.(0);
        hint = h.(1);
        items = Array.init 4 (fun x -> Word.make h.(x + 2) h.(0));
        difficulty = diff;
      }
      :: category_list_of_string_array_list diff t

let make_category_list file diff =
  category_list_of_string_array_list diff
    (separate_string_list (list_from_file file))

let make name hint i diff = { name = name; hint = hint; items = i; difficulty = diff }
let diff c = c.difficulty
let name c = c.name
let items c = c.items

let test_category =
  { name = "string"; hint = "string"; items = [||]; difficulty = "string" }

let test_category2 =
  {
    name = "string";
    hint = "string";
    items = [| Word.test_word |];
    difficulty = "string";
  }

let test_category3 =
  {
    name = "string";
    hint = "string";
    items = [| Word.test_word; Word.test_word2; Word.test_word3 |];
    difficulty = "string";
  }

let test_category4 =
  {
    name = "abcd";
    hint = "hi";
    items = [| Word.test_word; Word.test_word2; Word.test_word3 |];
    difficulty = "4";
  }
