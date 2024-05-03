open Connections
open Category
open Word

let () = Random.self_init ()
let yellow = make_category_list "yellow.txt" "yellow"
let green = make_category_list "green.txt" "green"
let blue = make_category_list "blue.txt" "blue"
let purple = make_category_list "purple.txt" "purple"
let size = List.length yellow
(* Testing list of four categories, and random number used to decide which
    categories from them are picked for a given time *)
let x = Random.int (size) 
let const = [
  (List.nth yellow x);
  (List.nth green x);
  (List.nth blue x);
  (List.nth purple x);
]
let shuffle a = for i = 0 to Array.length a * 20 do 
  let l = Random.int (Array.length a - 1) in let k = Random.int (Array.length a - 1) 
in let placeholder = a.(k) in a.(k) <- a.(l); a.(l) <- placeholder;
done 

(* Define a type to keep track of guessed words for each category *)
let guessed_words = Array.make 16 (Word.make "empty" "empty")
(* Helper function that converts a string of numbers (ex. "10 1 4 13") into an int list
    (ex. [10; 1; 4; 13])*)
let rec convert_to_int_list acc = function
| [] -> List.rev acc
| hd :: tl ->
    let num = int_of_string hd in
    convert_to_int_list (num :: acc) tl

(* Helper function to update guessed words for a category *)
let array_eliminate (l1 : Word.t array) (l2 : Word.t array) =
  let l2_list = Array.to_list l2 in
  let l1_list = Array.to_list l1 in
  let l2_filtered = (List.filter (fun word -> (word.word <> "empty")) l2_list) in
  (* let () = List.iter (fun word -> Printf.printf "%s " word.word) l2_filtered in *)
  (Array.of_list (List.filter (fun x -> not (List.mem x l2_filtered)) l1_list))

(* Helper function to update words_array with correctly guessed words *)
let update_words_array (words_array : Word.t array) (guessed_word : Word.t array) =
  let guessed_list = Array.to_list guessed_word in
  let guessed_filtered = Array.of_list ((List.filter (fun word -> word.word <> "empty")) guessed_list) in
  let remaining = array_eliminate words_array guessed_word in
  Array.append guessed_filtered remaining


let rec game num words_array (guessed_words : Word.t array) =
  (* Prints out the words in a stylized 4x4 grid.*)
  for words = 0 to (Array.length words_array - 1) do 
    if (words mod 4 = 0 && words <> 0) then 
      let () = print_newline () in
      ANSITerminal.printf [ANSITerminal.green] "[%i: %s]" (words + 1) (words_array.(words).word)
    else 
      ANSITerminal.printf [ANSITerminal.green] "[%i: %s]" (words + 1) (words_array.(words).word)
  done;

  (* User input *)
  let () = print_newline () in
  let () = print_endline "Please enter the numbers of the four words you would like to guess, with a space in between." in
  let the_input = read_line () in
  let numbers = 
    let split_input = String.split_on_char ' ' the_input in
    convert_to_int_list [] split_input in

  (* Compares the categories of the four word guesses. *)
  if List.length numbers = 4 then 
    let word1 = words_array.((List.nth numbers 0) - 1) in
    let word2 = words_array.((List.nth numbers 1) - 1) in
    let word3 = words_array.((List.nth numbers 2) - 1) in
    let word4 = words_array.((List.nth numbers 3) - 1) in
    let word1_category = word1.category in
    let word2_category = word2.category in
    let word3_category = word3.category in
    let word4_category = word4.category in
    
    if word1_category = word2_category && 
      word1_category = word3_category && 
      word1_category = word4_category then
      let () = print_endline "Correct!" in
      (* Update guessed words *)
      let () = guessed_words.(16 - num) <- word1 in
      let () = guessed_words.(17 - num) <- word2 in
      let () = guessed_words.(18 - num) <- word3 in
      let () = guessed_words.(19 - num) <- word4 in
      let guessed_list = Array.to_list guessed_words in
      let check_empty = List.filter (fun word -> (word.word = "empty")) guessed_list in
      if List.length check_empty = 0 then
        print_endline "You win!"
      else
        game (num - 4) (update_words_array words_array guessed_words) guessed_words
    else if (word1_category = word2_category &&
      word1_category = word3_category) || (word1_category = word2_category &&
      word1_category = word4_category) || (word1_category = word3_category &&
      word1_category = word4_category) || (word2_category = word3_category &&
      word2_category = word4_category)
      then 
      let three_category = 
        if word1_category = word2_category then word1_category
        else if word1_category = word3_category then word1_category
        else if word1_category = word4_category then word1_category
        else word2_category 
      in 
      let category_tried = List.find (fun x -> x.name = three_category) const in 
      let hint = category_tried.hint in 
      print_endline ("One Away! Hint: " ^ hint)
  else
    print_endline ("Enter 4 numbers for 4 words.")

let rec main_loop words_array =
  game (Array.length words_array) words_array guessed_words;
  print_endline "Do you want to play again? (yes/no)";
  match read_line () with
  | "yes" -> main_loop words_array
  | "no" -> print_endline "Thanks for playing!"
  | _ -> print_endline "Invalid input. Please enter 'yes' or 'no'."; main_loop words_array 

let _ = 
  let words_array = Array.make (List.length const * 4) (Word.make "" "") in
  (* Store all elements from all four categories into one [word array] *)
  for i = 0 to (List.length const) - 1 do
    for j = 0 to Array.length ((List.nth const i).items) - 1 do
      words_array.(4 * i + j) <- (List.nth const i).items.(j)
    done
  done;
  let () = shuffle words_array in
  main_loop words_array

