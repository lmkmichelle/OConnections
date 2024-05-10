open Category
open Word
let yellow = make_category_list "yellow.txt" "yellow"
let green = make_category_list "green.txt" "green"
let blue = make_category_list "blue.txt" "blue"
let purple = make_category_list "purple.txt" "purple"

let random_num_list = 
  [| Random.int (List.length yellow);
    Random.int (List.length yellow);
    Random.int (List.length yellow);
    Random.int (List.length yellow)
    |]

let guessed_words_init = Array.make 16 (Word.make "empty" "empty")
let const custom random_num_list = 
  if custom = "yellow" then
    [
    (List.nth yellow random_num_list.(0));
    (List.nth yellow random_num_list.(1));
    (List.nth yellow random_num_list.(2));
    (List.nth yellow random_num_list.(3));
    ]
  else if custom = "blue" then
    [
    (List.nth blue random_num_list.(0));
    (List.nth blue random_num_list.(1));
    (List.nth blue random_num_list.(2));
    (List.nth blue random_num_list.(3));
    ]
  else if custom = "green" then
    [
    (List.nth green random_num_list.(0));
    (List.nth green random_num_list.(1));
    (List.nth green random_num_list.(2));
    (List.nth green random_num_list.(3));
    ]
  else if custom = "purple" then 
    [
    (List.nth purple random_num_list.(0));
    (List.nth purple random_num_list.(1));
    (List.nth purple random_num_list.(2));
    (List.nth purple random_num_list.(3));
    ]
  else 
    [
    (List.nth yellow random_num_list.(0));
    (List.nth green random_num_list.(1));
    (List.nth blue random_num_list.(2));
    (List.nth purple random_num_list.(3));
    ]
let shuffle a =
  for _ = 0 to Array.length a * 20 do
    let l = Random.int (Array.length a - 1) in
    let k = Random.int (Array.length a - 1) in
    let placeholder = a.(k) in
    a.(k) <- a.(l);
    a.(l) <- placeholder
  done

let array_eliminate (l1 : Word.t array) (l2 : Word.t array) =
  let l2_list = Array.to_list l2 in
  let l1_list = Array.to_list l1 in
  let l2_filtered = (List.filter (fun word -> word.word <> "empty") l2_list) in
  (Array.of_list (List.filter (fun x -> not (List.mem x l2_filtered)) l1_list))

let update_words_array (words_array : Word.t array) (guessed_word : Word.t array) =
  let guessed_list = Array.to_list guessed_word in
  let guessed_filtered = 
    Array.of_list 
    ((List.filter (fun word -> word.word <> "empty")) guessed_list) 
  in
  let remaining = array_eliminate words_array guessed_word in
  Array.append guessed_filtered remaining

let check_win guessed_words = 
  let guessed_list = Array.to_list guessed_words in
      let check_empty = List.filter (fun word -> (word.word = "empty")) guessed_list in
      if List.length check_empty = 0 then
        true
      else false
        