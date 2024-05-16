open Category
open Word

let () = Random.self_init ()
let yellow = make_category_list "yellow.txt" "yellow"
let green = make_category_list "green.txt" "green"
let blue = make_category_list "blue.txt" "blue"
let purple = make_category_list "purple.txt" "purple"

let random_num_list =
  [|
    Random.int (List.length yellow);
    Random.int (List.length yellow);
    Random.int (List.length yellow);
    Random.int (List.length yellow);
  |]

let guessed_words_init = Array.make 16 (Word.make "empty" "empty")

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
  let l2_filtered = List.filter (fun word -> word.word <> "empty") l2_list in
  Array.of_list (List.filter (fun x -> not (List.mem x l2_filtered)) l1_list)

let update_words_array (words_array : Word.t array)
    (guessed_word : Word.t array) =
  let guessed_list = Array.to_list guessed_word in
  let guessed_filtered =
    Array.of_list
      ((List.filter (fun word -> word.word <> "empty")) guessed_list)
  in
  let remaining = array_eliminate words_array guessed_word in
  Array.append guessed_filtered remaining

let check_win guessed_words =
  let guessed_list = Array.to_list guessed_words in
  let check_empty =
    List.filter (fun word -> word.word = "empty") guessed_list
  in
  if List.length check_empty = 0 then true else false

let color_match i =
  if i = "yellow" then ANSITerminal.yellow
  else if i = "green" then ANSITerminal.green
  else if i = "blue" then ANSITerminal.blue
  else ANSITerminal.magenta

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

let rec contradiction_eliminator (l : Category.t list) =
  let final_list = ref l in
  let yellow' = List.nth l 0 in
  let green' = List.nth l 1 in
  let blue' = List.nth l 2 in
  let purple' = List.nth l 3 in
  (if name green' = name yellow' then
     final_list :=
       contradiction_eliminator
         [
           yellow';
           List.nth green (Random.int (List.length yellow));
           blue';
           purple';
         ]
   else if name blue' = name yellow' then
     final_list :=
       contradiction_eliminator
         [
           yellow';
           green';
           List.nth blue (Random.int (List.length yellow));
           purple';
         ]
   else if name blue' = name green' then
     final_list :=
       contradiction_eliminator
         [
           yellow';
           green';
           List.nth blue (Random.int (List.length yellow));
           purple';
         ]
   else if name purple' = name yellow' then
     final_list :=
       contradiction_eliminator
         [
           yellow';
           green';
           blue';
           List.nth purple (Random.int (List.length yellow));
         ]
   else if name purple' = name green' then
     final_list :=
       contradiction_eliminator
         [
           yellow';
           green';
           blue';
           List.nth purple (Random.int (List.length yellow));
         ]
   else if name purple' = name blue' then
     final_list :=
       contradiction_eliminator
         [
           yellow';
           green';
           blue';
           List.nth purple (Random.int (List.length yellow));
         ]
   else
     let category_list = [ yellow'; green'; blue'; purple' ] in
     for j = 1 to 3 do
       for i = 0 to 3 do
         for k = 0 to 3 do
           if (items yellow').(i) = (items (List.nth category_list j)).(k) then
             if k = 1 then
               final_list :=
                 contradiction_eliminator
                   [
                     yellow';
                     List.nth green (Random.int (List.length yellow));
                     blue';
                     purple';
                   ]
             else if k = 2 then
               final_list :=
                 contradiction_eliminator
                   [
                     yellow';
                     green';
                     List.nth blue (Random.int (List.length yellow));
                     purple';
                   ]
             else
               final_list :=
                 contradiction_eliminator
                   [
                     yellow';
                     green';
                     blue';
                     List.nth purple (Random.int (List.length yellow));
                   ]
         done
       done
     done;
     for j = 2 to 3 do
       for i = 0 to 3 do
         for k = 0 to 3 do
           if (items green').(i) = (items (List.nth category_list j)).(k) then
             if k = 2 then
               final_list :=
                 contradiction_eliminator
                   [
                     yellow';
                     green';
                     List.nth blue (Random.int (List.length yellow));
                     purple';
                   ]
             else
               final_list :=
                 contradiction_eliminator
                   [
                     yellow';
                     green';
                     blue';
                     List.nth purple (Random.int (List.length yellow));
                   ]
         done
       done
     done;
     for i = 0 to 3 do
       for k = 0 to 3 do
         if (items blue').(i) = (items purple').(k) then
           final_list :=
             contradiction_eliminator
               [
                 yellow';
                 green';
                 blue';
                 List.nth purple (Random.int (List.length yellow));
               ]
       done
     done);
  !final_list

exception OutsideArchiveBounds

let const custom random_num_list =
  if custom = "yellow" then
    [
      List.nth yellow random_num_list.(0);
      List.nth yellow random_num_list.(1);
      List.nth yellow random_num_list.(2);
      List.nth yellow random_num_list.(3);
    ]
  else if custom = "blue" then
    [
      List.nth blue random_num_list.(0);
      List.nth blue random_num_list.(1);
      List.nth blue random_num_list.(2);
      List.nth blue random_num_list.(3);
    ]
  else if custom = "green" then
    [
      List.nth green random_num_list.(0);
      List.nth green random_num_list.(1);
      List.nth green random_num_list.(2);
      List.nth green random_num_list.(3);
    ]
  else if custom = "purple" then
    [
      List.nth purple random_num_list.(0);
      List.nth purple random_num_list.(1);
      List.nth purple random_num_list.(2);
      List.nth purple random_num_list.(3);
    ]
  else if custom = "random" then
    contradiction_eliminator
      [
        List.nth yellow random_num_list.(0);
        List.nth green random_num_list.(1);
        List.nth blue random_num_list.(2);
        List.nth purple random_num_list.(3);
      ]
  else if String.contains custom 'A' then
    let day = int_of_string (String.sub custom 8 2) in
    if day < 00 || day > 39 then raise OutsideArchiveBounds
    else
      [
        List.nth yellow day;
        List.nth green day;
        List.nth blue day;
        List.nth purple day;
      ]
  else
    [
      List.nth yellow random_num_list.(0);
      List.nth green random_num_list.(0);
      List.nth blue random_num_list.(0);
      List.nth purple random_num_list.(0);
    ]
