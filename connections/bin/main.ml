open Connections
open Category
open Word

let () = Random.self_init ()

let yellow = make_category_list "yellow.txt" 1
let green = make_category_list "green.txt" 2
let blue = make_category_list "blue.txt" 3
let purple = make_category_list "purple.txt" 4
let size = List.length yellow
(** Testing list of four categories*)
let x = Random.int (size) 
let const = [
  (List.nth yellow x);
  (List.nth green x);
  (List.nth blue x);
  (List.nth purple x);
]



(** Helper function that converts a string of numbers (ex. "10 1 4 13") into an int list
    (ex. [10; 1; 4; 13])*)
let rec convert_to_int_list acc = function
| [] -> List.rev acc
| hd :: tl ->
    let num = int_of_string hd in
    convert_to_int_list (num :: acc) tl
  
(** Main game function.*)
let game () = 
  let words_array = Array.make 16 (Word.make "" "") in
  (* Store all elements from all four categories into one [word array] *)
  for i = 0 to (List.length const) - 1 do
    for j = 0 to Array.length ((List.nth const i).items) - 1 do
     words_array.(4 * i + j) <- (List.nth const i).items.(j)
    done
  done;
  let () = shuffle words_array in
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
  let word1_category = (words_array.((List.nth numbers 0) - 1)).category in
  let word2_category = (words_array.((List.nth numbers 1) - 1)).category in
  let word3_category = (words_array.((List.nth numbers 2) - 1)).category in
  let word4_category = (words_array.((List.nth numbers 3) - 1)).category in
  
  if word1_category = word2_category && 
     word1_category = word3_category && 
     word1_category = word4_category then
     print_endline "Correct!" 
    else if (word1_category = word2_category &&
      word1_category = word3_category) || (word1_category = word2_category &&
      word1_category = word4_category) || (word1_category = word3_category &&
      word1_category = word4_category) || (word2_category = word3_category &&
      word2_category = word4_category)
      then print_endline "One away!"
  else
    print_endline "Nope!" 
else
  print_endline ("Enter 4 numbers for 4 words.")

let _ = game ()

