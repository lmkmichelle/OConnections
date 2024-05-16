open Connections
open Category
open Word
open Game

let () = Random.self_init ()
let number_of_lives = ref 4

(* Helper function that converts a string of numbers (ex. "10 1 4 13") into an
   int list (ex. [10; 1; 4; 13])*)
let rec convert_to_int_list acc = function
  | [] -> List.rev acc
  | hd :: tl ->
      let num = int_of_string hd in
      convert_to_int_list (num :: acc) tl

let rec game num words_array (guessed_words : Word.t array) const hint_mode =
  (* Prints out the words in a stylized 4x4 grid.*)
  let guessed =
    ref
      (Array.find_index
         (function
           | x -> x = Word.make "empty" "empty")
         guessed_words)
    (* in let () = print_endline (string_of_int (Option.get guessed)) in *)
  in
  if guessed = ref None then guessed := Some 0;
  for i = 0 to Option.get !guessed - 1 do
    let () =
      if i mod 4 = 0 then
        let () = print_endline "" in
        ANSITerminal.print_string
          [ color_match (diff (category_find guessed_words.(i) const)) ]
          (name (category_find guessed_words.(i) const))
      else print_string ""
    in
    if i mod 4 = 0 then
      let () = print_newline () in
      ANSITerminal.printf
        [ color_match (diff (category_find guessed_words.(i) const)) ]
        "[%i: %s]" (i + 1) words_array.(i).word
    else
      ANSITerminal.printf
        [ color_match (diff (category_find guessed_words.(i) const)) ]
        "[%i: %s]" (i + 1) words_array.(i).word
  done;
  for words = Option.get !guessed to Array.length words_array - 1 do
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
       with a space in between. Please enter 'shuffle' if you would like to \
       shuffle the current board. "
  in
  let the_input = read_line () in
  if the_input = "shuffle" then
    let () = shuffle words_array in
    game num
      (update_words_array words_array guessed_words)
      guessed_words const hint_mode
  else
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
          print_endline
            "You can not guess the same word twice. Please try again."
        in
        game num words_array guessed_words const hint_mode
      else if
        Array.mem word1 guessed_words
        || Array.mem word2 guessed_words
        || Array.mem word3 guessed_words
        || Array.mem word4 guessed_words
      then
        let () =
          print_endline
            "You cannot guess words whose category has been \n\
            \        solved. Please try again."
        in
        game num words_array guessed_words const hint_mode
      else if
        word1_category = word2_category
        && word1_category = word3_category
        && word1_category = word4_category
      then
        let () = print_endline "Correct!" in
        let () =
          print_endline
            ("Number of tries remaining: " ^ string_of_int !number_of_lives)
        in

        (* Update guessed words *)
        let () = guessed_words.(16 - num) <- word1 in
        let () = guessed_words.(17 - num) <- word2 in
        let () = guessed_words.(18 - num) <- word3 in
        let () = guessed_words.(19 - num) <- word4 in
        (* Check if player has won *)
        if check_win guessed_words then print_endline "You win!"
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
        if !number_of_lives = 0 then
          print_endline "Out of tries. Better luck next time!"
        else
          let () = decr number_of_lives in
          let three_category =
            if word1_category = word2_category then word1_category
            else if word1_category = word3_category then word1_category
            else if word1_category = word4_category then word1_category
            else word2_category
          in
          let category_tried =
            List.find (fun x -> x.name = three_category) const
          in
          let hint = category_tried.hint in
          if hint_mode = "yes" then
            let () = print_endline ("One Away! Hint: " ^ hint) in
            let () =
              print_endline
                ("Number of tries remaining: " ^ string_of_int !number_of_lives)
            in
            game num words_array guessed_words const hint_mode
          else
            let () = print_endline "One Away!" in
            let () =
              print_endline
                ("Number of tries remaining: " ^ string_of_int !number_of_lives)
            in
            game num words_array guessed_words const hint_mode
      else if !number_of_lives = 0 then
        print_endline "Out of tries. Better luck next time!"
      else
        let () = decr number_of_lives in
        let () = print_endline "Nope!" in
        let () =
          print_endline
            ("Number of tries remaining: " ^ string_of_int !number_of_lives)
        in
        game num words_array guessed_words const hint_mode
    else print_endline "Enter 4 numbers for 4 words."

let rec main_loop const words_array guessed_words hint mode =
  game (Array.length words_array) words_array guessed_words const hint;
  if mode = "yellow" then (
    let () = print_endline "Do you want to play again? (yes/no)" in
    match read_line () with
    | "yes" ->
        let num_list =
          [| Random.int 40; Random.int 40; Random.int 40; Random.int 40 |]
        in
        let new_words_array = Array.make 16 (Word.make "" "") in
        (* Store all elements from all four categories into one array *)
        for i = 0 to List.length (Game.const "yellow" num_list) - 1 do
          for
            j = 0
            to Array.length (List.nth (Game.const "yellow" num_list) i).items
               - 1
          do
            new_words_array.((4 * i) + j) <-
              (List.nth (Game.const "yellow" num_list) i).items.(j)
          done
        done;
        Game.shuffle new_words_array;
        main_loop
          (Game.const "yellow" num_list)
          new_words_array 
          (Array.make 16 (Word.make "empty" "empty")) hint mode
    | "no" -> print_endline "Thanks for playing!"
    | _ ->
        print_endline "Invalid input. Please enter 'yes' or 'no'.";
        main_loop const words_array guessed_words hint mode)
  else if mode = "green" then (
    let () = print_endline "Do you want to play again? (yes/no)" in
    match read_line () with
    | "yes" ->
        let num_list =
          [| Random.int 40; Random.int 40; Random.int 40; Random.int 40 |]
        in
        let new_words_array =
          Array.make
            (List.length (Game.const "green" num_list) * 4)
            (Word.make "" "")
        in
        let new_const = Game.const "green" num_list in
        (* Store all elements from all four categories into one array *)
        for i = 0 to List.length new_const - 1 do
          for j = 0 to Array.length (List.nth new_const i).items - 1 do
            new_words_array.((4 * i) + j) <- (List.nth new_const i).items.(j)
          done
        done;
        Game.shuffle new_words_array;
        main_loop new_const new_words_array 
        (Array.make 16 (Word.make "empty" "empty")) hint mode
    | "no" -> print_endline "Thanks for playing!"
    | _ ->
        print_endline "Invalid input. Please enter 'yes' or 'no'.";
        main_loop const words_array guessed_words hint mode)
  else if mode = "blue" then (
    let () = print_endline "Do you want to play again? (yes/no)" in
    match read_line () with
    | "yes" ->
        let num_list =
          [| Random.int 40; Random.int 40; Random.int 40; Random.int 40 |]
        in
        let new_words_array =
          Array.make
            (List.length (Game.const "blue" num_list) * 4)
            (Word.make "" "")
        in
        (* Store all elements from all four categories into one array *)
        for i = 0 to List.length (Game.const "blue" num_list) - 1 do
          for
            j = 0
            to Array.length (List.nth (Game.const "blue" num_list) i).items - 1
          do
            new_words_array.((4 * i) + j) <-
              (List.nth (Game.const "blue" num_list) i).items.(j)
          done
        done;
        Game.shuffle new_words_array;
        main_loop
          (Game.const "blue" num_list)
          new_words_array 
          (Array.make 16 (Word.make "empty" "empty")) hint mode
    | "no" -> print_endline "Thanks for playing!"
    | _ ->
        print_endline "Invalid input. Please enter 'yes' or 'no'.";
        main_loop const words_array guessed_words hint mode)
  else if mode = "purple" then (
    let () = print_endline "Do you want to play again? (yes/no)" in
    match read_line () with
    | "yes" ->
        let num_list =
          [| Random.int 40; Random.int 40; Random.int 40; Random.int 40 |]
        in
        let new_words_array =
          Array.make
            (List.length (Game.const "purple" num_list) * 4)
            (Word.make "" "")
        in
        (* Store all elements from all four categories into one array *)
        for i = 0 to List.length (Game.const "purple" num_list) - 1 do
          for
            j = 0
            to Array.length (List.nth (Game.const "purple" num_list) i).items
               - 1
          do
            new_words_array.((4 * i) + j) <-
              (List.nth (Game.const "purple" num_list) i).items.(j)
          done
        done;
        Game.shuffle new_words_array;
        main_loop
          (Game.const "purple" num_list)
          new_words_array 
          (Array.make 16 (Word.make "empty" "empty")) hint mode
    | "no" -> print_endline "Thanks for playing!"
    | _ ->
        print_endline "Invalid input. Please enter 'yes' or 'no'.";
        main_loop const words_array guessed_words hint mode)
  else if mode = "normal" then (
    let () = print_endline "Do you want to play again? (yes/no)" in
    match read_line () with
    | "yes" ->
        let num_list =
          [| Random.int 40; Random.int 40; Random.int 40; Random.int 40 |]
        in
        let new_const = Game.const "normal" num_list in
        let new_words_array =
          Array.make (List.length new_const * 4) (Word.make "" "")
        in
        (* Store all elements from all four categories into one array *)
        for i = 0 to List.length new_const - 1 do
          for j = 0 to Array.length (List.nth new_const i).items - 1 do
            new_words_array.((4 * i) + j) <- (List.nth new_const i).items.(j)
          done
        done;
        Game.shuffle new_words_array;
        main_loop new_const new_words_array 
        (Array.make 16 (Word.make "empty" "empty")) hint "normal"
    | "no" -> print_endline "Thanks for playing!"
    | _ ->
        print_endline "Invalid input. Please enter 'yes' or 'no'.";
        main_loop const words_array guessed_words hint mode)
  else if mode = "random" then (
    let () = print_endline "Do you want to play again? (yes/no)" in
    match read_line () with
    | "yes" ->
        let num_list =
          [| Random.int 40; Random.int 40; Random.int 40; Random.int 40 |]
        in
        let new_const = Game.const "random" num_list in
        let new_words_array =
          Array.make (List.length new_const * 4) (Word.make "" "")
        in
        (* Store all elements from all four categories into one array *)
        for i = 0 to List.length new_const - 1 do
          for j = 0 to Array.length (List.nth new_const i).items - 1 do
            new_words_array.((4 * i) + j) <- (List.nth new_const i).items.(j)
          done
        done;
        Game.shuffle new_words_array;
        main_loop new_const new_words_array 
        (Array.make 16 (Word.make "empty" "empty")) hint "random"
    | "no" -> print_endline "Thanks for playing!"
    | _ ->
        print_endline "Invalid input. Please enter 'yes' or 'no'.";
        main_loop const words_array guessed_words hint mode)
  else
    let day = int_of_string (String.sub mode 8 2) in
    if day = 00 then (
      let () =
        print_endline "Do you want to play the next day's game? (yes/no)"
      in
      match read_line () with
      | "yes" ->
          let num_list =
            [| Random.int 40; Random.int 40; Random.int 40; Random.int 40 |]
          in
          let new_const = Game.const "Archive~01" num_list in
          let new_words_array =
            Array.make (List.length new_const * 4) (Word.make "" "")
          in
          (* Store all elements from all four categories into one array *)
          for i = 0 to List.length new_const - 1 do
            for j = 0 to Array.length (List.nth new_const i).items - 1 do
              new_words_array.((4 * i) + j) <- (List.nth new_const i).items.(j)
            done
          done;
          Game.shuffle new_words_array;
          main_loop new_const new_words_array 
          (Array.make 16 (Word.make "empty" "empty")) hint
            "Archive~01"
      | "no" -> print_endline "Thanks for playing!"
      | _ ->
          print_endline "Invalid input. Please enter 'yes' or 'no'.";
          main_loop const words_array guessed_words hint mode)
    else if day = 39 then (
      let () = print_endline "Do you want to play yesterday's game? (yes/no)" in
      match read_line () with
      | "yes" ->
          let num_list =
            [| Random.int 40; Random.int 40; Random.int 40; Random.int 40 |]
          in
          let new_const = Game.const "Archive~38" num_list in
          let new_words_array =
            Array.make (List.length new_const * 4) (Word.make "" "")
          in
          (* Store all elements from all four categories into one array *)
          for i = 0 to List.length new_const - 1 do
            for j = 0 to Array.length (List.nth new_const i).items - 1 do
              new_words_array.((4 * i) + j) <- (List.nth new_const i).items.(j)
            done
          done;
          Game.shuffle new_words_array;
          main_loop new_const new_words_array 
          (Array.make 16 (Word.make "empty" "empty")) hint
            "Archive~38"
      | "no" -> print_endline "Thanks for playing!"
      | _ ->
          print_endline "Invalid input. Please enter 'yes' or 'no'.";
          main_loop const words_array guessed_words hint mode)
    else
      print_endline
        "Do you want to play yesterday's game, tomorrow's game, or neither? \
         (y/t/n)";
    match read_line () with
    | "y" ->
        let num_list =
          [| Random.int 40; Random.int 40; Random.int 40; Random.int 40 |]
        in
        let new_const =
          if day < 11 then
            Game.const ("Archive~" ^ "0" ^ string_of_int (day + 1)) num_list
          else Game.const ("Archive~" ^ string_of_int (day - 1)) num_list
        in
        let new_words_array =
          Array.make (List.length new_const * 4) (Word.make "" "")
        in
        (* Store all elements from all four categories into one array *)
        for i = 0 to List.length new_const - 1 do
          for j = 0 to Array.length (List.nth new_const i).items - 1 do
            new_words_array.((4 * i) + j) <- (List.nth new_const i).items.(j)
          done
        done;
        Game.shuffle new_words_array;
        main_loop new_const new_words_array 
        (Array.make 16 (Word.make "empty" "empty")) hint
          ("Archive~" ^ string_of_int (day - 1))
    | "t" ->
        let num_list =
          [| Random.int 40; Random.int 40; Random.int 40; Random.int 40 |]
        in
        let new_const =
          if day < 9 then
            Game.const ("Archive~" ^ "0" ^ string_of_int (day + 1)) num_list
          else Game.const ("Archive~" ^ string_of_int (day + 1)) num_list
        in
        let new_words_array =
          Array.make (List.length new_const * 4) (Word.make "" "")
        in
        (* Store all elements from all four categories into one array *)
        for i = 0 to List.length new_const - 1 do
          for j = 0 to Array.length (List.nth new_const i).items - 1 do
            new_words_array.((4 * i) + j) <- (List.nth new_const i).items.(j)
          done
        done;
        Game.shuffle new_words_array;
        main_loop new_const new_words_array 
        (Array.make 16 (Word.make "empty" "empty")) hint
          ("Archive~" ^ string_of_int (day + 1))
    | "n" -> print_endline "Thanks for playing!"
    | _ ->
        print_endline "Invalid input. Please enter 'yes' or 'no'.";
        main_loop const words_array 
        (Array.make 16 (Word.make "empty" "empty")) hint mode

let _ =
  (* dune exec bin/main.exe <hint_mode> <custom_difficulty> *)
  if Array.length Sys.argv <> 3 then
    print_endline
      "Format: dune exec bin/main.exe <hint_mode> <custom_difficulty>\n\
      \    <mode> should be 'hint' or 'none', depending on if you want hints \
       or not.\n\
      \    <custom_difficulty> should be the color difficulty you want (green, \
       yellow, blue, purple), 'Archive~##' where ## is the number of the game \
       you want (00 - 39) or 'none', to play archive mode, \n\
      \    or 'normal' for standard Connections."
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
    main_loop const words_array guessed_words_init Sys.argv.(1) Sys.argv.(2)
