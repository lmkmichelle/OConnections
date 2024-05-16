open OUnit2
open Connections.Word
open Connections.Category
open Connections.Game

(*return the [n]th elements of [lst]*)
let rec test_helper n lst =
  match lst with
  | [] -> failwith "empty list"
  | h :: t ->
      if n = 0 then (
        print_category h;
        h)
      else test_helper (n - 1) t

let expected_blue =
  [
    {
      name = "Keyboard Keys";
      hint = "Input Commands";
      items =
        [|
          Connections.Word.make "Option" "Keyboard Keys";
          Connections.Word.make "Return" "Keyboard Keys";
          Connections.Word.make "Shift" "Keyboard Keys";
          Connections.Word.make "Tab" "Keyboard Keys";
        |];
      difficulty = "blue";
    };
    {
      name = "Magazines";
      hint = "You can find them in a waiting room.";
      items =
        [|
          Connections.Word.make "Essence" "Magazines";
          Connections.Word.make "People" "Magazines";
          Connections.Word.make "Time" "Magazines";
          Connections.Word.make "Us" "Magazines";
        |];
      difficulty = "blue";
    };
  ]

let expected_green =
  [
    {
      name = "NBA Teams";
      hint = "Ball is life!";
      items =
        [|
          Connections.Word.make "Bucks" "NBA Teams";
          Connections.Word.make "Heat" "NBA Teams";
          Connections.Word.make "Jazz" "NBA Teams";
          Connections.Word.make "Nets" "NBA Teams";
        |];
      difficulty = "green";
    };
    {
      name = "Units of Length";
      hint = "Europeans would instead use meter";
      items =
        [|
          Connections.Word.make "Foot" "Units of Length";
          Connections.Word.make "League" "Units of Length";
          Connections.Word.make "Mile" "Units of Length";
          Connections.Word.make "Yard" "Units of Length";
        |];
      difficulty = "green";
    };
  ]

let expected_purple =
  [
    {
      name = "Palindromes";
      hint = "abcdcba";
      items =
        [|
          Connections.Word.make "Kayak" "Palindromes";
          Connections.Word.make "Level" "Palindromes";
          Connections.Word.make "Mom" "Palindromes";
          Connections.Word.make "Racecar" "Palindromes";
        |];
      difficulty = "purple";
    };
    {
      name = "Letter Homophones";
      hint = "I see a bee";
      items =
        [|
          Connections.Word.make "Are" "Letter Homophones";
          Connections.Word.make "Queue" "Letter Homophones";
          Connections.Word.make "Sea" "Letter Homophones";
          Connections.Word.make "Why" "Letter Homophones";
        |];
      difficulty = "purple";
    };
  ]

let expected_yellow =
  [
    {
      name = "Wet Weather";
      hint = "Don't forget to bring your umbrella!";
      items =
        [|
          Connections.Word.make " Hail" "Wet Weather";
          Connections.Word.make " Rain" "Wet Weather";
          Connections.Word.make " Sleet" "Wet Weather";
          Connections.Word.make " Snow" "Wet Weather";
        |];
      difficulty = "yellow";
    };
    {
      name = "Footwear";
      hint = "Clothes for feet";
      items =
        [|
          Connections.Word.make "Boot" "Footwear";
          Connections.Word.make "Loafer" "Footwear";
          Connections.Word.make "Pump" "Footwear";
          Connections.Word.make "Sneaker" "Footwear";
        |];
      difficulty = "yellow";
    };
  ]

let init_lst =
  [|
    Connections.Word.make "empty" "empty";
    Connections.Word.make "empty" "empty";
    Connections.Word.make "empty" "empty";
    Connections.Word.make "empty" "empty";
    Connections.Word.make "empty" "empty";
    Connections.Word.make "empty" "empty";
    Connections.Word.make "empty" "empty";
    Connections.Word.make "empty" "empty";
    Connections.Word.make "empty" "empty";
    Connections.Word.make "empty" "empty";
    Connections.Word.make "empty" "empty";
    Connections.Word.make "empty" "empty";
    Connections.Word.make "empty" "empty";
    Connections.Word.make "empty" "empty";
    Connections.Word.make "empty" "empty";
    Connections.Word.make "empty" "empty";
  |]

let const_oab_test attempted =
  let ints = Array.make 4 0 in
  assert_equal
    (Some (Connections.Game.OutsideArchiveBounds { attempted }))
    (try
       let c =
         Connections.Game.const ("Archive~" ^ string_of_int attempted) ints
       in
       None
     with e -> Some e)

let tests =
  "tests"
  >::: [
         ( "word_make" >:: fun _ ->
           (*a basic test with a regular word and regular category*)
           assert_equal test_word (Connections.Word.make "hello" "greeting");
           (*test with a an empty string word and an empty string category*)
           assert_equal test_word2 (Connections.Word.make "" "");
           (*testing make with strings that have characters that are not
             letters*)
           assert_equal test_word3 (Connections.Word.make "%^z+3" "/s@_{}[]") );
         ( "word_is_empty" >:: fun _ ->
           (*testing an empty t (both the word and the category is empty) *)
           assert_bool "failed1" (is_empty test_word2);
           (*testing a t with an empty word but non-empty category *)
           let a = Connections.Word.make "" "category" in
           assert_bool "failed2" (is_empty a);
           (*testing a t with non-empty word and non-empty category *)
           assert_bool "failed3" (not (is_empty test_word));
           (*testing a t with a non-empty word and empty category*)
           let b = Connections.Word.make "word" "" in
           assert_bool "failed4" (not (is_empty b)) );
         ( "category_make" >:: fun _ ->
           (*creating a category with empty items*)
           assert_equal test_category (make "string" "string" [||] "string");
           (*creating a category with only one word in items*)
           assert_equal test_category2
             (make "string" "string" [| test_word |] "string");
           (*creating a category with multiple words in items*)
           assert_equal test_category3
             (make "string" "string"
                [| test_word; test_word2; test_word3 |]
                "string");
           (*using different strings for name, hint and difficulty fields*)
           assert_equal test_category4
             (make "abcd" "hi" [| test_word; test_word2; test_word3 |] "4") );
         ( "category_contains" >:: fun _ ->
           (*testing that the contains function does not detect any words within
             a category that has empty items field*)
           assert_bool "failed5" (not (contains test_word test_category));
           (*testing that the contains function does not detect an empty word
             within a category that has empty items field*)
           assert_bool "failed6" (not (contains test_word2 test_category));
           (*testing that contains function detects a word when it's in a
             category's items field *)
           assert_bool "failed7" (contains test_word test_category2);
           (*testing that contains function does not detect another word that is
             not existent in the items field *)
           assert_bool "failed8" (not (contains test_word2 test_category2));
           (*testing that contains function can still detect a word in items
             field of a category when the items field has multiple words*)
           assert_bool "failed9" (contains test_word test_category3);
           (*testing that contains function can detect all the word in items
             field of a category when the items field has multiple words*)
           assert_bool "failed10" (contains test_word2 test_category3);
           (*testing that contains function can detect all the word in items
             field of a category when the items field has multiple words*)
           assert_bool "failed10" (contains test_word3 test_category3) );
         ( "category_diff" >:: fun _ ->
           (*diff function finds the difficulty of the catgeory correctly**)
           assert_equal (diff test_category) "string";
           (*diff function finds the difficulty of the category correctly**)
           assert_equal (diff test_category4) "4" );
         ( "category_name" >:: fun _ ->
           (*name function finds the name of the catgeory correctly**)
           assert_equal (name test_category) "string";
           (*name function finds the name of the category correctly**)
           assert_equal (name test_category4) "abcd" );
         ( "category_items" >:: fun _ ->
           (*items function finds the items of the category correctly**)
           assert_equal (items test_category) [||];
           (*items function finds the items of the category correctly when there
             are multiple items**)
           assert_equal (items test_category4)
             [| test_word; test_word2; test_word3 |] );
         ( "category_make_category_list" >:: fun _ ->
           (*checks if the first element of the expected list is the same as the
             list created from blue.txt with the [make_category_list]
             function *)
           let category_list = make_category_list "blue.txt" "blue" in
           let first_el = test_helper 0 category_list in
           let first_el_expected = test_helper 0 expected_blue in
           assert_equal first_el first_el_expected;
           (*checks if the second element of the expected list is the same as
             the list created from blue.txt with the [make_category_list]
             function *)
           let second_el = test_helper 1 category_list in
           let second_el_expected = test_helper 1 expected_blue in
           assert_equal second_el second_el_expected;
           (*checks if the first element of the expected list is the same as the
             list created from green.txt with the [make_category_list]
             function *)
           let category_listg = make_category_list "green.txt" "green" in
           let first_elg = test_helper 0 category_listg in
           let first_el_expectedg = test_helper 0 expected_green in
           assert_equal first_elg first_el_expectedg;
           (*checks if the second element of the expected list is the same as
             the list created from green.txt with the [make_category_list]
             function *)
           let second_elg = test_helper 1 category_listg in
           let second_el_expectedg = test_helper 1 expected_green in
           assert_equal second_elg second_el_expectedg;
           (*checks if the first element of the expected list is the same as the
             list created from purple.txt with the [make_category_list]
             function *)
           let category_listp = make_category_list "purple.txt" "purple" in
           let first_elp = test_helper 0 category_listp in
           let first_el_expectedp = test_helper 0 expected_purple in
           assert_equal first_elp first_el_expectedp;
           (*checks if the second element of the expected list is the same as
             the list created from purple.txt with the [make_category_list]
             function *)
           let second_elp = test_helper 1 category_listp in
           let second_el_expectedp = test_helper 1 expected_purple in
           assert_equal second_elp second_el_expectedp;
           (*checks if the first element of the expected list is the same as the
             list created from yellow.txt with the [make_category_list]
             function *)
           let category_listy = make_category_list "yellow.txt" "yellow" in
           let first_ely = test_helper 0 category_listy in
           let first_el_expectedy = test_helper 0 expected_yellow in
           assert_equal first_ely first_el_expectedy;
           (*checks if the second element of the expected list is the same as
             the list created from yellow.txt with the [make_category_list]
             function *)
           let second_ely = test_helper 1 category_listy in
           let second_el_expectedy = test_helper 1 expected_yellow in
           assert_equal second_ely second_el_expectedy );
         ( "game_guessed_words_init" >:: fun _ ->
           assert_equal init_lst guessed_words_init );
         ( "exception OutsideArchiveBounds with const" >:: fun _ ->
           (*const run on archive with input > 39 raises OutsideArchiveBounds
             correctly**)
           const_oab_test 40;
           (*const run on archive with negative input raises
             OutsideArchiveBounds correctly**)
           const_oab_test ~-10 );
       ]

let () = run_test_tt_main tests
