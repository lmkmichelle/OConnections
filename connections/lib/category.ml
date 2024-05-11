type t = {
  name : string;
  hint : string;
  items : Word.t array;
  difficulty : string;
}

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

let make s1 s2 i s3 = { name = s1; hint = s2; items = i; difficulty = s3 }
let diff c = c.difficulty
let name c = c.name
let items c = c.items
(* let test_category = [ { name = "Category1"; hint = "Hint of Category1"; items
   = [| make "word1" "Category1"; make "word2" "Category1"; make "word3"
   "Category1"; |]; difficulty = 1; }; ] *)
