open Connections
open Category
open Word
open Game

let () = Random.self_init ()

let guessed_words = guessed_words_init
let number_of_lives = ref 4

(* Helper function that converts a string of numbers (ex. "10 1 4 13") into an int list
    (ex. [10; 1; 4; 13])*)
let rec convert_to_int_list acc = function
  | [] -> List.rev acc
  | hd :: tl ->
      let num = int_of_string hd in
      convert_to_int_list (num :: acc) tl

let rec game num words_array (guessed_words : Word.t array) const hint_mode =


let color_match i =
  if i = "yellow" then ANSITerminal.yellow
  else if i = "green" then ANSITerminal.green
  else if i = "blue" then ANSITerminal.blue
  else ANSITerminal.magenta
(**takes string i, which represents the difficulty of a category and returns the 
    corresponding ANSITerminal.style for it to be printed in the right color*)
let category_find e (l : Category.t list) =
  let x =
    ref
      (Category.make "none" "none"
         (Array.make 4 (Word.make "none" "none"))
         "none")
  in
  for i = 0 to 3 do
    if contains e (List.nth l i) then x := List.nth l i
  done;
  !x
(**searches through a list l of categories to find which one contains element e*)
let rec game num words_array (guessed_words : Word.t array) =
  (* Prints out the words in a stylized 4x4 grid.*)
  let guessed =
    Array.find_index
      (function
        | x -> x = Word.make "empty" "empty")
      guessed_words
  (* in let () = print_endline (string_of_int (Option.get guessed)) in *)
  in
  for i = 0 to Option.get guessed - 1 do
    let () = if i mod 4 = 0 
      then let () = 
    print_endline "" in print_endline (name (category_find guessed_words.(i) const)) 
      else print_string "" 
    in
    if i mod 4 = 0 then 
    let () = print_newline () in
    ANSITerminal.printf
      [ color_match (diff (category_find guessed_words.(i) const)) ]
      "[%i: %s]" (i + 1) words_array.(i).word
else ANSITerminal.printf
[ color_match (diff (category_find guessed_words.(i) const)) ]
"[%i: %s]" (i + 1) words_array.(i).word
  done;
  for words = Option.get guessed to Array.length words_array - 1 do
    if words mod 4 = 0 && words <> 0 then
      let () = print_newline () in
      ANSITerminal.printf [ ANSITerminal.black ] "[%i: %s]" (words + 1)
        words_array.(words).word
    else
      ANSITerminal.printf [ ANSITerminal.black ] "[%i: %s]" (words + 1)
        words_array.(words).word
  done;

  (* User input *)
  let () = print_newline () in
  let () =
    print_endline
      "Please enter the numbers of the four words you would like to guess, \
       with a space in between."
  in
  let the_input = read_line () in
  let numbers =
    let split_input = String.split_on_char ' ' the_input in
    convert_to_int_list [] split_input
  in

  (* Compares the categories of the four word guesses. *)
  if List.length numbers = 4 then
    let word1 = words_array.(List.nth numbers 0 - 1) in
    let word2 = words_array.(List.nth numbers 1 - 1) in
    let word3 = words_array.(List.nth numbers 2 - 1) in
    let word4 = words_array.(List.nth numbers 3 - 1) in
    let word1_category = word1.category in
    let word2_category = word2.category in
    let word3_category = word3.category in
    let word4_category = word4.category in
    if
      word1.word = word2.word || word1.word = word3.word
      || word1.word = word4.word || word2.word = word3.word
      || word2.word = word4.word || word3.word = word4.word
    then
      let () =
        print_endline "You can not guess the same word twice. Please try again."
      in
      game num words_array guessed_words const hint_mode
    else if
      word1_category = word2_category
      && word1_category = word3_category
      && word1_category = word4_category
    then
      let () = print_endline "Correct!" in
      (* Update guessed words *)
      let () = guessed_words.(16 - num) <- word1 in
      let () = guessed_words.(17 - num) <- word2 in
      let () = guessed_words.(18 - num) <- word3 in
      let () = guessed_words.(19 - num) <- word4 in
      if check_win guessed_words then
        print_endline "You win!"
      else
        game (num - 4)
          (update_words_array words_array guessed_words)
          guessed_words const hint_mode
    else if
      (word1_category = word2_category && word1_category = word3_category)
      || (word1_category = word2_category && word1_category = word4_category)
      || (word1_category = word3_category && word1_category = word4_category)
      || (word2_category = word3_category && word2_category = word4_category)
    then
      if !number_of_lives = 0 then print_endline ("Out of tries. Better luck next time!")
      else 
      let () = decr number_of_lives in
      let three_category =
        if word1_category = word2_category then word1_category
        else if word1_category = word3_category then word1_category
        else if word1_category = word4_category then word1_category
        else word2_category
      in
      let category_tried = List.find (fun x -> x.name = three_category) const in
      let hint = category_tried.hint in
      if hint_mode = "yes" then
        let () = print_endline ("One Away! Hint: " ^ hint) in
        game num words_array guessed_words const hint_mode
      else
        let () = print_endline ("One Away!") in
        let () = print_endline ("Number of tries remaining: " ^ (string_of_int !number_of_lives)) in
      game num words_array guessed_words const hint_mode
    else 
      if !number_of_lives = 0 then print_endline ("Out of tries. Better luck next time!")
      else 
      let () = decr number_of_lives in
      let () = print_endline ("Nope!") in
      let () = print_endline ("Number of tries remaining: " ^ (string_of_int !number_of_lives)) in
      game num words_array guessed_words const hint_mode

    else print_endline "Enter 4 numbers for 4 words."

let rec main_loop const words_array hint =
  
  game (Array.length words_array) words_array guessed_words const hint;
  print_endline "Do you want to play again? (yes/no)";
  match read_line () with
  | "yes" -> main_loop const words_array hint
  | "no" -> print_endline "Thanks for playing!"
  | _ ->
      print_endline "Invalid input. Please enter 'yes' or 'no'.";
      main_loop const words_array hint

let _ =
  (* dune exec bin/main.exe <hint_mode> <custom_difficulty> *)
  if Array.length Sys.argv <> 3 then
    print_endline ("Format: dune exec bin/main.exe <hint_mode> <custom_difficulty>
    <hint_mode> should be 'yes' or 'no', depending on if you want hints.
    <custom_difficulty> should be the color difficulty you want (green, yellow, blue, purple), 
    or 'normal' for standard Connections.")
  else
  let const = const Sys.argv.(2) random_num_list in
  let words_array = Array.make (List.length const * 4) (Word.make "" "") in
  (* Store all elements from all four categories into one [word array] *)
  for i = 0 to List.length const - 1 do
    for j = 0 to Array.length (List.nth const i).items - 1 do
      words_array.((4 * i) + j) <- (List.nth const i).items.(j)
    done
  done;
  let () = shuffle words_array in
  main_loop const words_array Sys.argv.(1)
