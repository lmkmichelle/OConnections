open Connections
open Category
open Word
open Game

let () = Random.self_init ()
let const = const random_num 
let guessed_words = guessed_words_init

(* Helper function that converts a string of numbers (ex. "10 1 4 13") into an int list
    (ex. [10; 1; 4; 13])*)
let rec convert_to_int_list acc = function
| [] -> List.rev acc
| hd :: tl ->
    let num = int_of_string hd in
    convert_to_int_list (num :: acc) tl

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
      if check_win guessed_words then
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

